#' Process chat message into standard format
#'
#' This function takes one or more chat messages and processes them into a
#' standard list format with role and content for each message to be fed to a
#' large language model.
#'
#' The standard format is a list of chat messages with the following structure:
#' message: `c(role = "system", content = "Welcome to the chat!")`
#'
#' list of messages: \code{list(
#'    c(role = "system", content = "You are an useful AI assistant."),
#'    c(role = "user", content = "Hi there!")
#'  )}
#'
#'  list format: \code{list(
#'    list(role = "system", content = "You are an useful AI assistant."),
#'    list(role = "user", content = "Hi there!")
#'  )}
#'
#' @param messages A character vector or list of chat messages. It can be a
#'   vector or a specifically structured list.
#'
#' @return A list of chat messages in standard format.
#'
#' @importFrom utils hasName
#'
#' @examples
#' process_messages(c(user = "Hello, AI!"))
#' process_messages(list(list(role = "user", content = "How are you?")))
#'
#' @export
process_messages <- function(messages) {
  if (missing(messages) || is.null(messages) || length(messages) == 0) {
    stop("User messages are required.")
  }

  # Assume that a single message is from the user
  if (
    length(messages) == 1 &&
      is.character(messages) &&
      is.null(names(messages))
  ) {
    messages <- c(user = messages)
  }

  # Convert vector to list format
  vector_to_list <- function(msg_vec) {
    # Check if vector is in named format
    check <- all(
      names(msg_vec) %in%
        c("system", "user", "assistant", "function"),
      na.rm = TRUE
    )

    check <- check && !is.null(names(msg_vec))

    if (check) {
      # Convert from vector to list format
      msg_vec <- purrr::imap(msg_vec, function(msg, nm) {
        list(role = nm, content = msg)
      }) |>
        stats::setNames(NULL)
    } else {
      stop("Invalid format for 'messages' vector.")
    }
  }

  # Validate list format
  validate_list_format <- function(msg_list) {
    # Check if the message is in correct list format
    check <- !purrr::every(msg_list, function(msg) {
      vctrs::obj_is_list(msg) &&
        hasName(msg, "role") &&
        hasName(msg, "content") &&
        msg$role %in% c("system", "user", "assistant", "function")
    })

    return(!check)
  }

  # Check if message is in a valid vector format
  if (is.character(messages)) {
    return(vector_to_list(messages))
  }

  if (vctrs::obj_is_list(messages)) {
    # Check if valid list format
    if (validate_list_format(messages)) {
      return(messages)
    }

    # It turned out the API doesn't really support batch calls of
    # multiple prompts

    # # Check if list of vectors
    # if (purrr::every(messages, is.character)) {
    #
    #   # Convert each to list
    #   return(purrr::map(messages, vector_to_list))
    #
    # }
    #
    # # Check if list of lists
    # if (purrr::every(messages, validate_list_format)) {
    #
    #   return(messages)
    #
    # }
  }

  stop("Message is neither a valid vector nor a valid list.")
}

#' Interrogate a Language Model
#'
#' This function sends requests to a specified language model provider and
#' returns the response. It handles rate limiting, retries requests if
#' necessary, and processes errors in the response.
#'
#' @param messages Messages to be sent to the language model. Can be a single
#'   string, a named vector, or a list of messages. See `process_messages()` for
#'   details.
#' @param provider The provider of the language model. Default is set by the
#'   `llmr_llm_provider` option. Supported providers include "openai", "azure",
#'   "gemini", and "custom".
#' @param params Additional parameters for the language model request. These
#'   will override any default parameters stored with the model specification.
#' @param model_specification Either a model label (string) that has been
#'   registered with `record_llmr_model()`, or a complete model specification
#'   list containing the same fields as `record_llmr_model()`. If provided, this
#'   overrides the current model set by `set_llmr_model()`.
#' @param force_json A boolean to force the response in JSON format. Default is
#'   FALSE. Note: This is not supported by all providers.
#' @param log_request A boolean to log the request time. Default is set by the
#'   `llmr_log_requests` option.
#' @param session_id The LLM session ID to store the data under. If not set, a
#'   new one will be created.
#' @param ... Additional arguments passed to the language model provider
#'   functions.
#'
#' @section Error Handling: The function automatically handles rate limit errors
#'   and will retry the request after waiting for the specified time. For
#'   responses cut off due to token limits, the function will attempt to
#'   complete the response, which may involve user interaction.
#'
#' @section JSON Output: When `force_json = TRUE`, the function attempts to get
#'   a JSON response from the LLM. If parsing fails, it tries to sanitize the
#'   output.
#'
#' @examples
#' \dontrun{
#' # First record a model specification
#' record_llmr_model(
#'   label = "openai",
#'   provider = "openai",
#'   model = "gpt-4",
#'   api_key = "your_api_key",
#'   parameters = list(temperature = 0.7)
#' )
#'
#' # Then use the registered model label
#' response <- prompt_llm(
#'   # Shorthand message format
#'   messages = c(user = "Hello there!"), # or simply "Hello there!"
#'   model_specification = "openai"
#' )
#'
#' # Using a complete model specification
#' response <- prompt_llm(
#'   # Canonical message format
#'   messages = list(list(role = "user", content = "What's the weather?")),
#'   model_specification = list(
#'     provider = "openai",
#'     model = "gpt-4",
#'     api_key = "your_api_key",
#'     parameters = list(temperature = 0.7)
#'   )
#' )
#' }
#'
#' @export
prompt_llm <- function(
  messages = NULL,
  provider = getOption("llmr_llm_provider"),
  params = list(),
  model_specification = NULL,
  force_json = FALSE,
  log_request = getOption("llmr_log_requests", TRUE),
  session_id = get_session_id() %||% set_session_id(),
  ...
) {
  messages <- process_messages(messages)

  # Initialize model_spec to NULL. It will be populated if model_specification
  # is used, or if a current_model is set via options.
  model_spec <- NULL
  provider_args_from_spec <- list()

  if (!is.null(model_specification)) {
    if (is.character(model_specification)) {
      stored_models <- getOption("llmr_stored_models", list())
      if (!(model_specification %in% names(stored_models))) {
        stop(
          "No stored model specification under label '",
          model_specification,
          "'."
        )
      }
      model_spec <- stored_models[[model_specification]]
    } else if (is.list(model_specification)) {
      model_spec <- model_specification
    } else {
      stop(
        "'model_specification' must be either a character string (label) or a ",
        "list (specification)"
      )
    }

    # When model_specification is provided, it dictates these primary parameters
    provider <- model_spec$provider
    if (!is.null(model_spec$parameters)) {
      params <- utils::modifyList(model_spec$parameters, params)
    }

    # Prepare arguments for the provider function based on model_spec. These
    # will override anything from getOption() within the provider functions
    provider_args_from_spec$model <- model_spec$model
    provider_args_from_spec$api_key <- model_spec$api_key
    provider_args_from_spec$endpoint <- model_spec$endpoint
    provider_args_from_spec$api_version <- model_spec$api_version
    # For Azure compatibility, map model to deployment_id and endpoint to
    # resource_name
    if (provider == "azure") {
      provider_args_from_spec$deployment_id <- model_spec$model
      provider_args_from_spec$resource_name <- model_spec$endpoint
      # Remove generic model/endpoint if azure specific ones are present
      provider_args_from_spec$model <- NULL
      provider_args_from_spec$endpoint <- NULL
    }
  } else {
    # No model_specification provided, try to use current model from options
    current_model_label <- getOption("llmr_current_model")
    if (!is.null(current_model_label)) {
      stored_models <- getOption("llmr_stored_models", list())
      if (current_model_label %in% names(stored_models)) {
        model_spec <- stored_models[[current_model_label]] # For session logging
        # Merge stored parameters with provided ones
        if (!is.null(model_spec$parameters)) {
          params <- utils::modifyList(model_spec$parameters, params)
        }
      }
    }
    # Provider remains getOption("llmr_llm_provider") or the argument default
    # Provider functions will use their getOption() defaults for model, api_key
    # etc.
  }

  if (is.null(provider)) {
    stop(
      "Language model provider is not set. ",
      "You can use set_llmr_model() or provide model_specification."
    )
  }

  body <- params
  body$messages <- messages

  if (force_json) {
    body$response_format <- list("type" = "json_object")
  }

  llm_fun_name <- paste0("use_", provider, "_llm")
  if (!exists(llm_fun_name, mode = "function")) {
    stop(
      "Unsupported LLM provider: ",
      provider,
      "\nYou can set it project-wide using the llmr_llm_provider option."
    )
  }
  llm_fun <- get(llm_fun_name)

  # Prepare the arguments for the provider function call
  call_args <- list(body = body)

  # Add arguments derived from model_specification (if any)
  # These take precedence and are specific to what the provider function expects
  # Filter out NULLs from provider_args_from_spec before merging
  provider_args_from_spec <- purrr::compact(provider_args_from_spec)

  # Merge ... arguments, giving precedence to those from model_spec
  # and then to those explicitly passed in ...
  # This is tricky: ... might contain model, api_key etc. that should be ignored
  # if model_spec provided them. A safer approach: define what each provider
  # function takes and only pass those.

  # Let's build call_args by respecting model_spec first, then ..., then
  # function defaults All provider functions take 'log_request'
  call_args$log_request <- log_request

  # Add specific args based on provider, from provider_args_from_spec or ... If
  # model_specification was used, provider_args_from_spec contains its details.
  # Otherwise, provider functions pick up from global options.

  extra_args <- list(...)

  if (provider == "openai") {
    call_args$model <- provider_args_from_spec$model %||%
      extra_args$model %||%
      getOption("llmr_model")
    call_args$api_key <- provider_args_from_spec$api_key %||%
      extra_args$api_key %||%
      getOption("llmr_api_key")
  } else if (provider == "azure") {
    call_args$deployment_id <- provider_args_from_spec$deployment_id %||%
      extra_args$deployment_id %||%
      getOption("llmr_model")
    call_args$resource_name <- provider_args_from_spec$resource_name %||%
      extra_args$resource_name %||%
      getOption("llmr_endpoint")
    call_args$api_key <- provider_args_from_spec$api_key %||%
      extra_args$api_key %||%
      getOption("llmr_api_key")
    call_args$api_version <- provider_args_from_spec$api_version %||%
      extra_args$api_version %||%
      getOption("llmr_api_version")
  } else if (provider == "custom") {
    call_args$endpoint <- provider_args_from_spec$endpoint %||%
      extra_args$endpoint %||%
      getOption("llmr_endpoint")
    call_args$model <- provider_args_from_spec$model %||%
      extra_args$model %||%
      getOption("llmr_model")
    call_args$api_key <- provider_args_from_spec$api_key %||%
      extra_args$api_key %||%
      getOption("llmr_api_key")
  } else if (provider == "gemini") {
    call_args$model <- provider_args_from_spec$model %||%
      extra_args$model %||%
      getOption("llmr_model")
    call_args$api_key <- provider_args_from_spec$api_key %||%
      extra_args$api_key %||%
      getOption("llmr_api_key")
  } else if (provider == "mock") {
    call_args$model <- provider_args_from_spec$model %||%
      extra_args$model %||%
      "FakeLLama"
    # Mock specific params from ... if provided
    if (!is.null(extra_args$response)) {
      call_args$response <- extra_args$response
    }
    if (!is.null(extra_args$error_msg)) {
      call_args$error_msg <- extra_args$error_msg
    }
    if (!is.null(extra_args$status)) {
      call_args$status <- extra_args$status
    }
    if (!is.null(extra_args$retry_after)) {
      call_args$retry_after <- extra_args$retry_after
    }
    if (!is.null(extra_args$simul_delay)) {
      call_args$simul_delay <- extra_args$simul_delay
    }
  }

  # Remove any NULL arguments from call_args to prevent issues with do.call
  # Especially for parameters that might not be set by options or model_spec
  call_args <- purrr::compact(call_args)

  retry <- FALSE
  response_obj <- NULL

  while (is.null(response_obj) || retry) {
    retry <- FALSE
    gen_start <- Sys.time()

    # Ensure that only arguments accepted by llm_fun are passed
    # Get formal arguments of the llm_fun
    llm_fun_formals <- names(formals(llm_fun))

    # Filter call_args to include only those present in llm_fun_formals or if
    # llm_fun accepts ...
    final_call_args <- call_args[names(call_args) %in% llm_fun_formals]
    if ("..." %in% llm_fun_formals) {
      # Add back any arguments from original call_args that were not in
      # formals (intended for ...) This assumes ... arguments were not part
      # of provider_args_from_spec, which is mostly true A cleaner way is to
      # ensure `extra_args` are only those not already handled. For now, pass
      # all of call_args if ... is present. This might be too broad. Let's be
      # more specific: only pass non-formal args from 'extra_args' if '...' is
      # present
      potential_dots_args <- extra_args[!names(extra_args) %in% llm_fun_formals]
      final_call_args <- c(final_call_args, potential_dots_args)
    } else {
      # If no ..., check for unexpected arguments
      unexpected_args <- names(call_args)[
        !names(call_args) %in% llm_fun_formals
      ]
      if (length(unexpected_args) > 0) {
        # This case should ideally be avoided by the specific if/else for
        # providers above For safety, we'll warn and remove them if they are
        # not handled by the provider block logic.
        warning(paste(
          "Unexpected arguments for provider",
          provider,
          ":",
          paste(unexpected_args, collapse = ", ")
        ))
      }
    }

    # Ensure `body` is always the first argument if not explicitly named if
    # provider functions expect it positionally All current provider functions
    # have `body` as their first named argument. So `final_call_args` having
    # `body` as a named element is correct.

    response_obj <- do.call(llm_fun, final_call_args)
    gen_stop <- Sys.time()

    elapsed <- difftime(gen_stop, gen_start)
    retry <- is_rate_limit_exceeded(response_obj, log_request = log_request)
  }

  stop_on_response_error(response_obj)
  parsed <- httr::content(response_obj, as = "parsed", encoding = "UTF-8")
  stop_on_no_output(parsed)

  if (log_request) {
    if (force_json) {
      body$response_format <- "JSON" # For logging only
    }
    log_params <- body[setdiff(names(body), "messages")]
    with(
      parsed$usage,
      paste(
        "Params:",
        jsonlite::toJSON(log_params, auto_unbox = TRUE),
        "\nGeneration time:",
        format_timediff(elapsed),
        "\nPrompt tokens:",
        prompt_tokens,
        "\nResponse tokens:",
        completion_tokens,
        "\nGeneration speed:",
        paste(
          signif(completion_tokens / as.numeric(elapsed, "secs"), 3),
          "t/s"
        ),
        "\nTotal tokens:",
        total_tokens
      )
    ) |>
      message()
  }

  llm_answer <- purrr::imap_chr(parsed$choices, \(ans, i) {
    ans_content <- ans$message$content
    if (force_json && !jsonlite::validate(ans_content)) {
      ans_content <- sanitize_json_output(ans_content)
    }
    if (ans$finish_reason == "length") {
      # ... (incomplete answer handling code remains the same) ...
      i <- if (length(parsed$choices) > 1) paste0(" ", i, " ") else " "

      opt_name <- Sys.time()

      message(
        "\nAnswer",
        i,
        "exhausted the context window!\n",
        "The incomplete answer has been saved as: ",
        opt_name,
        "in the `llmr_incomplete_answers` option object.\n",
      )

      # Store the incomplete answer in the option object
      incomplete_ans_list <- getOption("llmr_incomplete_answers", list())
      incomplete_ans_list[opt_name] <- ans_content
      options(llmr_incomplete_answers = incomplete_ans_list)

      # Collect the user's choice on how to proceed from the options
      choice <- getOption("llmr_continue_on_incomplete", NULL)

      choices <- c(
        "Try to complete the answer",
        "Keep the incomplete answer",
        "Stop the process"
      )

      # Default to complete the answer if not interactive and no choice was set
      # in the options
      if (!interactive() && is.null(choice)) {
        choice <- 1
      }

      if (!is.null(choice)) {
        message(
          sprintf(
            '\nWill follow the option %d: "%s"\n',
            choice,
            choices[choice]
          )
        )
      }

      # Otherwise, if interactive and no choice was set in the options, ask the
      # user how to proceed
      if (interactive() && is.null(choice)) {
        choice <- utils::menu(choices, title = "How do you want to proceed?")
      }

      if (choice == 1) {
        # Ask the model to continue the answer
        messages_new <- c(
          messages,
          list(list(
            role = "assistant",
            content = ans_content
          )),
          list(list(
            role = "user",
            content = "continue"
          ))
        )

        # Prepare arguments for recursive call, ensuring model_specification is
        # passed if used initially
        recursive_call_args <- list(
          messages = messages_new,
          force_json = FALSE, # Usually not forced for continuation
          params = params, # Pass current params
          log_request = log_request,
          session_id = session_id,
          provider = provider # Provider is already correctly set
        )

        # If original call used model_specification, pass it along, otherwise
        # provider functions will use options
        if (!is.null(model_specification)) {
          recursive_call_args$model_specification <- model_specification
        } else {
          # If model_specification was NULL, but ... might have contained model
          # details for specific provider, we need to pass those. The
          # provider-specific args (model, api_key, etc.) should be passed if
          # they were in `...`. This part gets tricky; the simplest is to rely
          # on the fact that if model_specification was NULL, the provider and
          # its params are already set up (either by options or initial ...) We
          # might need to reconstruct relevant parts of `...` if they are not
          # covered by model_spec/options. For now, let's assume if
          # model_specification was NULL, the existing provider context (from
          # options or initial ...) is sufficient. The `extra_args` list (...
          # from parent call) could be passed again. However, the recursive
          # prompt_llm will again resolve ... based on its own context.
          # Simplest: if model_specification was used, pass it. Otherwise, let
          # the recursive call use its defaults/options or specific ... args.
          # The current ... in scope here are for the *current* call, not to be
          # confused with parent's ...
        }

        # Add original ... arguments to the recursive call if they don't
        # conflict This ensures settings like a specific `model` passed via
        # `...` originally are respected if `model_specification` was not used.
        # We need to be careful not to pass ... that are already handled by
        # recursive_call_args
        original_dots <- list(...)
        for (arg_name in names(original_dots)) {
          if (!arg_name %in% names(recursive_call_args)) {
            recursive_call_args[[arg_name]] <- original_dots[[arg_name]]
          }
        }

        ans_new <- do.call(prompt_llm, recursive_call_args)

        return(paste0(ans_content, ans_new))
      } else if (choice == 2) {
        return(ans_content)
      } else {
        stop("The process has been stopped.")
      }
    } else {
      ans_content
    }
  })

  # Determine model name for logging: from model_spec if used, else from
  # options/dots
  logged_model_name <- NULL
  if (!is.null(model_spec)) {
    logged_model_name <- model_spec$model
  } else {
    # Fallback to options or ... if model_spec was not used. This depends on how
    # `model` was passed or set by options for the specific provider. It might
    # be in call_args$model or call_args$deployment_id
    logged_model_name <- call_args$model %||%
      call_args$deployment_id %||%
      getOption("llmr_model")
  }

  store_llm_session_data(
    messages = messages,
    response = llm_answer,
    usage = parsed$usage,
    processing_time = elapsed,
    provider = provider,
    model = logged_model_name,
    session_id = session_id
  )

  llm_answer
}

#' Use OpenAI Language Model
#'
#' Sends a request to the OpenAI API using the parameters in the `body`
#' argument. It requires an API key and model identifier set in the R options.
#' Users would not typically call this function directly, but rather use the
#' `prompt_llm` function.
#'
#' @param body The body of the request.
#' @param model Model identifier for the OpenAI API. Obtained from R options.
#' @param api_key API key for the OpenAI service. Obtained from R options.
#' @param log_request A boolean to log the request time. Can be set up globally
#'   using the `llmr_log_requests` option, which defaults to TRUE.
#'
#' @return The function returns the response from the OpenAI API.
#'
#' @export
use_openai_llm <- function(
  body,
  model = getOption("llmr_model"),
  api_key = getOption("llmr_api_key"),
  log_request = getOption("llmr_log_requests", TRUE)
) {
  if (is.null(api_key) || is.null(model)) {
    stop(
      "OpenAI GPT model or API key are not set. ",
      "Use the following options to set them:\n",
      "llmr_model, ",
      "llmr_api_key options."
    )
  }

  if (log_request) {
    message("Interrogating OpenAI: ", model, "...")
  }

  body$model <- model

  # Prepare the request
  httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(
      `Content-Type` = "application/json",
      `Authorization` = paste0("Bearer ", api_key)
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json"
  )
}

#' Use Azure Language Model
#'
#' Sends a request to the Azure API for language model completions using the
#' parameters in the `body` argument. This function requires specific Azure
#' configurations (deployment ID, resource name, API key, and API version) set
#' in the R options. Users would not typically call this function directly, but
#' rather use the `prompt_llm` function.
#'
#' @param body The body of the request.
#' @param deployment_id Azure deployment ID for the language model. Obtained
#'   from R options.
#' @param resource_name Azure resource name. Obtained from R options.
#' @param api_key API key for the Azure language model service. Obtained from R
#'   options.
#' @param api_version API version for the Azure language model service. Obtained
#'   from R options.
#' @param log_request A boolean to log the request time. Can be set up globally
#'   using the `llmr_log_requests` option, which defaults to TRUE.
#'
#' @return The function returns the response from the Azure API.
#'
#' @export
use_azure_llm <- function(
  body,
  deployment_id = getOption("llmr_model"),
  resource_name = getOption("llmr_endpoint"),
  api_key = getOption("llmr_api_key"),
  api_version = getOption("llmr_api_version"),
  log_request = getOption("llmr_log_requests", TRUE)
) {
  if (
    is.null(resource_name) ||
      is.null(deployment_id) ||
      is.null(api_key) ||
      is.null(api_version)
  ) {
    stop(
      "Azure GPT deployment name (model), resource name (endpoint), ",
      "API key, or API version are not set. ",
      "Use the following options to set them:\n",
      "llmr_model, ",
      "llmr_endpoint, ",
      "llmr_api_key, ",
      "llmr_api_version."
    )
  }

  if (log_request) {
    message(
      "Interrogating Azure OpenAI: ",
      resource_name,
      "/",
      deployment_id,
      " (",
      api_version,
      ")..."
    )
  }

  # Prepare the request
  httr::POST(
    url = paste0(
      "https://",
      resource_name,
      ".openai.azure.com/openai/deployments/",
      deployment_id,
      "/chat/completions?api-version=",
      api_version
    ),
    httr::add_headers(`Content-Type` = "application/json", `api-key` = api_key),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )
}

#' Use Custom Language Model
#'
#' Sends a request to a custom (local or remote) language model endpoint
#' compatible with the OpenAI API specification, using the parameters in the
#' `body` argument. The user can provide an API key if required. Users would not
#' typically call this function directly, but rather use the `prompt_llm`
#' function.
#'
#' @param body The body of the request.
#' @param endpoint The local endpoint for the language model service. Can be set
#'   up globally using the `llmr_endpoint` option.
#' @param model Model identifier for the custom API, if needed (some API have
#'   one model per endpoint, some multiple ones). Can be set up globally using
#'   the `llmr_model` option.
#' @param api_key Optional API key for the custom language model services that
#'   require it. Can be set up globally using the `llmr_api_key` option.
#' @param log_request A boolean to log the request time. Can be set up globally
#'   using the `llmr_log_requests` option, which defaults to `TRUE`.
#'
#' @return The function returns the response from the local language model
#'   endpoint.
#'
#' @export
use_custom_llm <- function(
  body,
  endpoint = getOption("llmr_endpoint"),
  model = getOption("llmr_model"),
  api_key = getOption("llmr_api_key"),
  log_request = getOption("llmr_log_requests", TRUE)
) {
  if (is.null(endpoint)) {
    stop(
      "Local endpoint is not set. ",
      "Use the following options to set it:\n",
      "llmr_endpoint"
    )
  }

  if (log_request) {
    message("Interrogating custom LLM: ", endpoint, "/", model, "...")
  }

  if (!is.null(model)) {
    body$model <- model
  }

  # Prepare the request
  httr::POST(
    url = endpoint,
    httr::add_headers(
      `Content-Type` = "application/json",
      if (!is.null(api_key)) {
        .headers = c(Authorization = paste0("Bearer ", api_key))
      }
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )
}

#' Use Google Gemini Language Model
#'
#' Sends a request to the Google Gemini API using the parameters in the `body`
#' argument. It requires an API key set in the R options. Internally, the body
#' and the output are transformed to be compatible with the OpenAI API format.
#'
#' @param body The body of the request, in OpenAI API format.
#' @param model Model identifier for the Google Gemini API. Obtained from R
#'   options.
#' @param api_key API key for the Google Gemini service. Obtained from R
#'   options.
#' @param log_request A boolean to log the request time. Default is set by the
#'   `llmr_log_requests` option.
#'
#' @return The function returns the response from the Google Gemini API,
#'   transformed to match the OpenAI API response format.
#'
#' @note This function internally transforms the request body from OpenAI format
#'   to Gemini format, and the response from Gemini format back to OpenAI
#'   format.
#'
#' @export
use_gemini_llm <- function(
  body,
  model = getOption("llmr_model"),
  api_key = getOption("llmr_api_key"),
  log_request = getOption("llmr_log_requests", TRUE)
) {
  if (is.null(api_key) || is.null(model)) {
    stop(
      "Google Gemini model or API key are not set. ",
      "Use the following options to set them:\n",
      "llmr_model, ",
      "llmr_api_key options."
    )
  }

  if (log_request) {
    message("Interrogating Google Gemini: ", model, "...")
  }

  url <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    model,
    ":generateContent?key=",
    api_key
  )

  # Extract system instruction if present
  system_instruction <- NULL
  other_messages <- list()

  for (msg in body$messages) {
    if (msg$role == "system") {
      system_instruction <- msg$content
    } else {
      role <- if (msg$role == "assistant") "model" else msg$role
      other_messages <- append(
        other_messages,
        list(list(
          role = role,
          parts = list(text = msg$content)
        ))
      )
    }
  }

  # Prepare the body of the request
  formatted_body <- list(
    contents = other_messages
  )

  # Add system instruction if present
  if (!is.null(system_instruction)) {
    formatted_body$system_instruction <- list(
      parts = list(text = system_instruction)
    )
  }

  # Check if the response should be in JSON format
  force_json <- !purrr::pluck(
    body,
    "response_format",
    "type",
    .default = FALSE
  ) %in%
    FALSE

  # Add parameters to the body if present
  formatted_body$generation_config <- list(
    temperature = body$temperature,
    maxOutputTokens = body$max_tokens,
    topP = body$top_p,
    topK = body$top_k,
    #candidateCount = body$params$n, # Not supported yet, produces empty answer
    response_mime_type = if (force_json) "application/json" else NULL
  ) |>
    purrr::compact()

  # Prepare the request
  trial <- 1
  while (trial < 4) {
    response <- httr::POST(
      url = url,
      httr::add_headers(
        `Content-Type` = "application/json"
      ),
      body = jsonlite::toJSON(formatted_body, auto_unbox = TRUE),
      encode = "json"
    )

    # Check for errors in response
    stop_on_response_error(response)

    # Parse and adjust the response format to be compatible with prompt_llm
    parsed_response <- httr::content(
      response,
      as = "parsed",
      encoding = "UTF-8"
    )

    if (parsed_response$candidates[[1]]$finishReason != "RECITATION") {
      break
    }

    trial <- trial + 1
    message("Failed due to RECITATION. Attempt ", trial, " of 4.")
  }

  metadata <- parsed_response$usageMetadata

  adjusted_response <- list(
    choices = lapply(parsed_response$candidates, function(candidate) {
      list(
        message = list(
          content = candidate$content$parts[[1]]$text
        ),
        finish_reason = candidate$finishReason
      )
    }),
    usage = list(
      prompt_tokens = metadata$promptTokenCount,
      completion_tokens = metadata$candidatesTokenCount,
      total_tokens = metadata$totalTokenCount
    )
  )

  # Transform the adjusted response back into an httr response object
  response$content <- charToRaw(
    jsonlite::toJSON(adjusted_response, auto_unbox = TRUE)
  )

  response
}


#' Mock Language Model call for testing
#'
#' This function mocks a call to a language model provider and returns a
#' predefined response. It is used for testing purposes to avoid making actual
#' requests to the language model provider. The function simulates a delay in
#' the response to mimic the actual response time.
#'
#' @param body The body of the request.
#' @param model The model identifier for the mock response. Defaults to
#'   "FakeLLama" (which obviously doesn't exist).
#' @param response The response to be returned by the mock call. The response
#'   can be set globally using the `llmr_mock_fun_response` option.
#' @param error_msg An error message to be returned in the response. It can be a
#'   string or a list. If it is a string, it is converted to a list with the
#'   message key. If not provided, the function generates a default error
#'   message based on the status code.
#' @param log_request A boolean to log the request time. Can be set up globally
#'   using the `llmr_log_requests` option, which defaults to TRUE.
#' @param status The status code of the response. Defaults to 200. It is used to
#'   simulate errors in the response. The status code can be set globally using
#'   the `llmr_mock_fun_status` option.
#' @param retry_after The time to wait before retrying the request when the
#'   status code is 429 (rate limit error). Defaults to 1 second. It can be set
#'   globally using the `llmr_mock_fun_retry_after` option.
#' @param simul_delay The delay in the response to simulate the actual response
#'   time. Defaults to 0.05 seconds. It can be set globally using the
#'   `llmr_mock_fun_simul_delay` option.
#'
#' @return The function returns the mock response.
#'
#' @examples
#'
#' \dontrun{
#' response <- use_mock_llm(
#' body = list(messages = list(list(role = "user", content = "Hello"))),
#' model = "FakeLLama",
#' response = "Test successful!"
#' )
#'
#' print(response)
#'
#' }
#'
#' @export
use_mock_llm <- function(
  body,
  model = "FakeLLama",
  response = getOption("llmr_mock_fun_response", "Test successful!"),
  error_msg = NULL,
  log_request = getOption("llmr_log_requests", TRUE),
  status = getOption("llmr_mock_fun_status", 200),
  retry_after = getOption("llmr_mock_fun_retry_after", 1),
  simul_delay = getOption("llmr_mock_fun_simul_delay", .05)
) {
  # Simulate a delay in the response
  Sys.sleep(simul_delay)

  if (log_request) {
    message("Interrogating MockLLM: ", model, "...")
  }

  # Build the mock response
  response_content <- list(
    choices = list(list(
      index = 0,
      message = list(role = "assistant", content = response),
      logprobs = NULL,
      finish_reason = "stop"
    )),
    usage = list(
      prompt_tokens = 10,
      completion_tokens = 20,
      total_tokens = 30
    )
  )

  # Add error message if present
  if (!is.null(error_msg)) {
    if (is.list(error_msg)) {
      response_content$error <- error_msg
    } else {
      response_content$error <- list(message = error_msg)
    }
  }

  headers <- list("Content-Type" = "application/json")

  if (status == 429) {
    message("Simulating error: 429")
    headers$`retry-after` <- retry_after

    if (is.null(response_content$error)) {
      response_content$error <- list(message = "Mock rate limit error.")
    }
  }

  if (status == 400) {
    message("Simulating error: 400")

    if (is.null(response_content$error)) {
      response_content$error <- list(message = "Mock general error.")
    }
  }

  if (status == 500) {
    message("Simulating error: 500")

    if (is.null(response_content$error)) {
      response_content$error <- list(message = "Mock server error.")
    }
  }

  response <- list(
    url = "http://www.fakellama.com",
    status_code = as.integer(status), # Ensure status_code is integer
    headers = headers,
    content = charToRaw(
      jsonlite::toJSON(
        response_content,
        auto_unbox = TRUE
      )
    ) # Encode content as raw vector
  )

  # Transform the response into a response object
  class(response) <- "response"

  response
}
