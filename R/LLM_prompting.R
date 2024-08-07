#' Process chat message into standard format
#'
#' This function takes one or more (a list of) chat messages and processes them
#' into a standard list format with role and content for each message to be fed
#' to a large language model.
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
#'  multiple parallel prompts: \code{list(
#'    list(
#'      list(role = "system", content = "You are an useful AI assistant."),
#'      list(role = "user", content = "Hi there!")
#'    ),
#'    list(
#'      list(role = "system", content = "You are an useful AI assistant."),
#'      list(role = "user", content = "Hi there!")
#'    )
#'  )}
#'
#' @param messages A character vector or list of chat messages. In can be a
#'   vector, a specifically structured list or a list of both if the goal is the
#'   have the API process multiple messages at once.
#'
#' @return A list of chat messages in standard format.
#'
#' @importFrom utils hasName
#'
process_messages <- function(messages) {

  if (missing(messages) || is.null(messages) || length(messages) == 0) {
    stop("User messages are required.")
  }

  # Assume that a single message is from the user
  if (length(messages) == 1 &&
      is.character(messages) &&
      is.null(names(messages))) {
    messages <- c(user = messages)
  }

  # Convert vector to list format
  vector_to_list <- function(msg_vec) {

    # Check if vector is in named format
    check <- all(
      names(msg_vec) %in%
        c("system", "user", "assistant", "function")
      , na.rm = TRUE)

    check <- check && !is.null(names(msg_vec))

    if (check) {

      # Convert from vector to list format
      msg_vec <- purrr::imap(msg_vec, function(msg, nm) {
        list(role = nm, content = msg)
      }) |> stats::setNames(NULL)

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
#' This function sends requests to a specified language model provider (OpenAI,
#' Azure, or a locally running LLM server) and returns the response. It handles
#' rate limiting and retries the request if necessary, and also processes errors
#' in the response.
#'
#' Users can provide their own models by writing a function with the following
#' name pattern: `use_<model_name>_llm`. See the existing functions using the
#' ::: operator for examples.
#'
#' @param messages Messages to be sent to the language model.
#' @param provider The provider of the language model. Maps to a specific
#'   function with the pattern "use_<provider>_llm. Default is set up globally
#'   using the `llmr_llm_provider` option. The package implements interfaces for
#'   models respecting the OpenAI (`openai`), Azure (`azure`), and Google Gemini
#'   (`gemini`) API specifications plus a general interface for custom models
#'   (`custom`) which implement the OpenAI API specification; see
#'   `use_<provider>_llm` functions.
#' @param params Additional parameters for the language model request. Defaults
#'   to a list with `temperature = 0`.
#' @param force_json A boolean to force the response in JSON format. Default is
#'   FALSE. Works only for OpenAI and Azure endpoints.
#' @param log_request A boolean to log the request time. Can be set up globally
#'   using the `llmr_log_requests` option, which defaults to TRUE.
#' @param session_id The LLM session ID to store the data under. NOTE: this ID
#'   is not used to continue a conversation keeping memory of the previous
#'   interactions with the LLM, but it's just to keep a copy of the conversation
#'   for review and post-processing.
#' @param ... Additional arguments passed to the language model provider
#'   functions.
#'
#' @return Returns the content of the message from the language model response.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' response <- prompt_llm(
#'  messages = c(user = "Hello there!"),
#'  provider = "openai")
#'  }
#'
prompt_llm <- function(
    messages = NULL,
    provider = getOption("llmr_llm_provider"),
    params = list(
      temperature = 0
    ),
    force_json = FALSE,
    log_request = getOption("llmr_log_requests", TRUE),
    session_id = getOption("llmr_session_id", NULL),
    ...
) {

  messages <- process_messages(messages)

  if (is.null(provider)) {
    stop("Language model provider is not set. ",
         "You can use the following option to set it globally:\n",
         "llmr_llm_provider.")
  }

  # Set default temperature if not provided
  if (!"temperature" %in% names(params)) {
    params$temperature <- 0
  }

  # Prepare the body of the request and merge with default
  body <- params

  body$messages <- messages

  # Force the LLM to answer in JSON format (not all models support this)
  if (force_json) {
    body$response_format <- list("type" = "json_object")
  }

  # Map provider to specific function
  llm_fun <- paste0("use_", provider, "_llm")

  if (!exists(llm_fun, mode = "function")) {
    stop("Unsupported LLM provider: ", provider,
         "\nYou can set it project-wide using the llmr_llm_provider option.")
  }

  llm_fun <- get(llm_fun)

  # Try to send the request
  retry <- FALSE

  while(!exists("response", inherits = FALSE) || retry) {

    retry <- FALSE

    gen_start <- Sys.time()
    response <- llm_fun(body, ...)
    gen_stop <- Sys.time()

    # Compute the time taken for the request to be processed
    elapsed <- difftime(gen_stop, gen_start)

    # Check if the query rate limit has been exceeded and wait for the time
    # specified in the response error message
    retry <- is_rate_limit_exceeded(response, log_request = log_request)
  }

  # Check for errors in response
  stop_on_response_error(response)

  # Return the response
  parsed <- httr::content(response, as = "parsed", encoding = "UTF-8")

  # Raise an error if no tokens were generated
  stop_on_no_output(parsed)

  if (log_request) {
    if (force_json) {
      params$response_format <- "JSON"
    }

    # Log information about the generated response
    with(parsed$usage,
         paste(
           "Params:", jsonlite::toJSON(params, auto_unbox = T),
           "\nGeneration time:", format_timediff(elapsed),
           "\nPrompt tokens:", prompt_tokens,
           "\nResponse tokens:", completion_tokens,
           "\nGeneration speed:", paste(
             signif(completion_tokens/as.numeric(elapsed, "secs"), 3), "t/s"),
           "\nTotal tokens:", total_tokens
         )
    ) |> message()

  }

  # Return the response
  llm_answer <- purrr::imap_chr(parsed$choices, \(ans, i) {
    ans_content <- ans$message$content

    # Manage the case when the answer is cut off due to exceeding the
    # output token limit
    if (ans$finish_reason == "length") {
      i <- if (length(parsed$choices) > 1) paste0(" ", i, " ") else " "

      opt_name <- Sys.time()

      message(
        "\nAnswer", i, "exhausted the context window!\n",
        "The incomplete answer has been saved as: ", opt_name,
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
            '\nWill follow the option %d: "%s"\n', choice, choices[choice])
        )
      }

      # Otherwise, if interactive and no choice was set in the options, ask the
      # user how to proceed
      if (interactive() && is.null(choice)) {
        choice <- utils::menu(choices,
          title = "How do you want to proceed?"
        )
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

        ans_new <- prompt_llm(
          messages_new,
          force_json = FALSE,
          params = params,
          log_request = log_request,
          session_id = session_id,
          provider = provider,
          ...
        )

        # TODO: how to manage the session history in these cases?

        return(paste0(ans_content, ans_new))
      } else if (choice == 2) {
        return(ans_content)
      } else {
        stop("The process has been stopped.")
      }
    } else ans_content
  })

  # Store the interaction in the session history
  store_llm_session_data(
    messages = messages,
    response = llm_answer,
    usage = parsed$usage,
    processing_time = elapsed,
    provider = provider,
    model = list(...)$model,
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
    stop("OpenAI GPT model or API key are not set. ",
         "Use the following options to set them:\n",
         "llmr_model, ",
         "llmr_api_key options.")
  }

  if (log_request) {
    message("Interrogating OpenAI: ", model, "...")
  }

  body$model = model

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
    deployment_id = getOption("llmr_azure_gpt_deployment"),
    resource_name = getOption("llmr_azure_gpt_resource"),
    api_key = getOption("llmr_api_key"),
    api_version = getOption("llmr_azure_api_version"),
    log_request = getOption("llmr_log_requests", TRUE)
) {

  if (is.null(resource_name) || is.null(deployment_id) ||
      is.null(api_key) || is.null(api_version)) {
    stop("Azure GPT resource name, deployment name, ",
         "API key, or API version are not set. ",
         "Use the following options to set them:\n",
         "llmr_azure_gpt_deployment, ",
         "llmr_azure_gpt_resource, ",
         "llmr_api_key, ",
         "llmr_azure_api_version."
    )
  }

  if (log_request) {
    message(
      "Interrogating Azure OpenAI: ", resource_name, "/", deployment_id,
      " (", api_version, ")...")
  }

  # Prepare the request
  httr::POST(
    url = paste0(
      "https://",
      resource_name,
      ".openai.azure.com/openai/deployments/",
      deployment_id,
      "/chat/completions?api-version=",
      api_version),
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
    stop("Local endpoint is not set. ",
         "Use the following options to set it:\n",
         "llmr_endpoint"
    )
  }

  if (log_request) {
    message("Interrogating custom LLM: ", endpoint, "/", model, "...")
  }

  if (!is.null(model)) {
    body$model = model
  }

  # Prepare the request
  httr::POST(
    url = endpoint,
    httr::add_headers(
      `Content-Type` = "application/json",
      if (!is.null(api_key)) {
        .headers = c(Authorization = paste0("Bearer ", api_key))
      }),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )

}

#' Use Google Gemini Language Model
#'
#' Sends a request to the Google Gemini API using the parameters in the `body`
#' argument. It requires an API key set in the R options. Internally, the body
#' and the output are transformed to be compatible with the OpenAI API. Users
#' would not typically call this function directly, but rather use the
#' `prompt_llm` function.
#'
#' @param body The body of the request.
#' @param model Model identifier for the Google Gemini API. Obtained from R
#'   options.
#' @param api_key API key for the Google Gemini service. Obtained from R
#'   options.
#' @param log_request A boolean to log the request time. Can be set up globally
#'   using the `llmr_log_requests` option, which defaults to TRUE.
#'
#' @return The function returns the response from the Google Gemini API.
#'
#' @export
use_gemini_llm <- function(
    body,
    model = getOption("llmr_model"),
    api_key = getOption("llmr_api_key"),
    log_request = getOption("llmr_log_requests", TRUE)
) {

  if (is.null(api_key) || is.null(model)) {
    stop("Google Gemini model or API key are not set. ",
         "Use the following options to set them:\n",
         "llmr_model, ",
         "llmr_api_key options.")
  }

  if (log_request) {
    message("Interrogating Google Gemini: ", model, "...")
  }

  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model, ":generateContent?key=", api_key)

  # Extract system instruction if present
  system_instruction <- NULL
  other_messages <- list()

  for (msg in body$messages) {
    if (msg$role == "system") {
      system_instruction <- msg$content
    } else {
      role <- if (msg$role == "assistant") "model" else msg$role
      other_messages <- append(other_messages, list(list(
        role = role,
        parts = list(text = msg$content)
      )))
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
    body, "response_format", "type", .default = FALSE) %in% FALSE

  # Add parameters to the body if present
  formatted_body$generation_config <- list(
    temperature = body$temperature,
    maxOutputTokens = body$max_tokens,
    topP = body$top_p,
    topK = body$top_k,
    #candidateCount = body$params$n, # Not supported yet, produces empty answer
    response_mime_type = if (force_json) "application/json" else NULL
  ) |> purrr::compact()

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
      response, as = "parsed", encoding = "UTF-8")

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
    jsonlite::toJSON(adjusted_response, auto_unbox = TRUE))

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
      response_content$error = error_msg
    } else {
      response_content$error = list(message = error_msg)
    }
  }

  headers <- list("Content-Type" = "application/json")

  if (status == 429) {
    message("Simulating error: 429")
    headers$`retry-after` = retry_after

    if (is.null(response_content$error)) {
      response_content$error = list(message = "Mock rate limit error.")
    }
  }

  if (status == 400) {
    message("Simulating error: 400")

    if (is.null(response_content$error)) {
      response_content$error = list(message = "Mock general error.")
    }
  }

  if (status == 500) {
    message("Simulating error: 500")

    if (is.null(response_content$error)) {
      response_content$error = list(message = "Mock server error.")
    }
  }

  response <- list(
    url = "http://www.fakellama.com",
    status_code = as.integer(status),  # Ensure status_code is integer
    headers = headers,
    content = charToRaw(
      jsonlite::toJSON(
        response_content, auto_unbox = TRUE))  # Encode content as raw vector
  )

  # Transform the response into a response object
  class(response) <- "response"

  response

}
