#' Format a difftime object as a string
#'
#' Formats a difftime object as a string with a specified precision.
#'
#' @param td A difftime object.
#' @param precision The number of significant digits to include in the output.
#'
#' @return A string representation of the difftime object.
#'
format_timediff <- function(td, precision = 3) {
  if (!inherits(td, "difftime")) stop("Input must be a difftime object.")

  paste(signif(unclass(td), precision), units(td))
}

#' Set the LLM session ID
#'
#' Set the session ID to store the data from the interaction with a language
#' model.
#'
#' @param session_id The session ID to store the data under. If not provided, a
#'   new one will be generated. Usually a number prepend with a hash (#).
#'
#' @return The session ID, invisibly.
#'
#' @export
set_session_id <- function(
    session_id = NULL
) {
  if (is.null(session_id)) {
    session_id <- paste0("#", as.numeric(Sys.time()))
  }

  options(llmr_session_id = session_id)

  invisible(session_id)
}

#' Store LLM session data
#'
#' Store information about the interaction with a language model in a session
#' option.
#'
#' @param messages A list of messages sent to the language model.
#' @param parameters A list of parameters used in the request.
#' @param response The response from the language model as a string or a vector.
#' @param usage The token usage information from the response.
#' @param processing_time The time taken to process the request.
#' @param provider The language model provider.
#' @param model The language model used.
#' @param session_id The LLM session ID to store the data under. Can be used to continue a session.
#'
#' @return NULL
#'
#' @export
#'
store_llm_session_data <- function(
    messages,
    parameters = NULL,
    response,
    usage = NULL,
    processing_time = NULL,
    provider = getOption("llmr_provider", NULL),
    model = getOption("llmr_model", NULL),
    session_id = getOption("llmr_session_id", NULL)
) {

  # If no session ID exist yet, generate a new one
  # Ensure a session ID exists
  session_id <- session_id %||% set_session_id()

  # Extract the number of tokens generated and the input tokens
  input_tokens <- usage$prompt_tokens
  output_tokens <- usage$completion_tokens

  # Extract the processing time as seconds
  if (inherits(processing_time, "difftime")) {
    processing_time <- as.numeric(processing_time, "secs")
  }
  if (is.na(as.numeric(processing_time))) {
    stop(
      "Processing time should be either a number of seconds or ",
      "a 'difftime' object")
  }

  # Calculate the generation speed
  gen_speed <- output_tokens/processing_time

  # Get the current timestamp
  ts <- Sys.time()

  # Store the data in a list, named by the timestamp
  new_session_data <- list(mget(
    c("ts", "messages", "parameters", "response", "input_tokens",
      "output_tokens", "processing_time", "gen_speed", "provider", "model"),
  )) |> stats::setNames(ts)

  # Get the current session data
  current_session_data <- getOption("llmr_session_data", list())

  # If the session ID does not exist yet, create a new list
  if (!session_id %in% names(current_session_data)) {
    current_session_data[[session_id]] <- list()
  }

  # Append the new data to the session data
  current_session_data[[session_id]] <- append(
    current_session_data[[session_id]], new_session_data)

  # Store the updated session data
  options(llmr_session_data = current_session_data)
}

#' Get LLM session ID
#'
#' Retrieve the current session ID stored in the session option.
#'
#' @return The current session ID.
#'
#' @export
get_session_id <- function() {
  getOption("llmr_session_id", NULL)
}

#' Get LLM session data
#'
#' Retrieve the data stored in the session option for a specific session ID.
#'
#' @param id The session ID to retrieve the data for. If not provided, all
#'   session data will be returned.
#'
#' @return A list with the session data or a list of lists of session data.
#'
#' @export
get_session_data <- function(id = NULL) {
  llmr_session_data <- getOption("llmr_session_data", list())

  if (is.null(id)) {
    llmr_session_data
  } else {
    llmr_session_data[[id]]
  }
}

#' Get LLM session data IDs
#'
#' Retrieve the session IDs for the stored session data.
#'
#' @return A character vector with the session IDs.
#'
#' @export
get_session_data_ids <- function() {
  names(getOption("llmr_session_data", list()))
}


#' Reset LLM session data
#'
#' Reset the session data stored in the session option. If a session ID is
#' provided, only the data for that session will be reset.
#'
#' @param id The session ID to reset the data for. If not provided, all session
#'   data will be reset.
#'
#' @return NULL
#'
#' @export
remove_session_data <- function(id = NULL) {
  if (is.null(id)) {
    options(llmr_session_data = list())
  } else {
    llmr_session_data <- get_session_data()

    if (length(llmr_session_data) == 0) {
      warning("No session history is present yet")
      return()
    }
    
    if (!(id %in% names(llmr_session_data))) {
      warning("Session ID not present in session history.",
      call. = FALSE, immediate. = TRUE)
      return()
    }
    
    llmr_session_data[id] <- NULL
    options(llmr_session_data = llmr_session_data)
  }
}

#' Store LLM model specifications
#'
#' Store the specifications for a language model in the global options.
#'
#' @param provider The provider name under which to store the model
#'   specifications.
#' @param api_type The type of API used by the provider (custom, openai, azure,
#'   or gemini).
#' @param endpoint The API endpoint to use for the provider.
#' @param api_key The API key to use for the provider.
#' @param model The default model to use for the provider. Some providers may
#'   accept only one model, while others do not require a model to be specified.
#' @param api_version The version of the API to use (only for "azure" api types
#'   for now)
#'
#' @return NULL
#'
#' @export
record_llmr_model <- function(
  provider,
  api_type = c("custom", "openai", "azure", "gemini"),
  endpoint = NULL,
  api_key = NULL,
  model = NULL,
  api_version = NULL
) {

  api_type <- match.arg(api_type)

  # Get current stored model specifications
  cur_specs <- getOption("llmr_stored_models", list())

  # Store model data
  cur_specs[[provider]] <- mget(
    c("api_type", "endpoint", "api_key", "model", "api_version")
  )

  # Store the options
  options(llmr_stored_models = cur_specs)
}

#' Set the current LLMR model
#'
#' This function allows you to set the current LLMR model to use for subsequent
#' operations. It retrieves the stored model specifications for the given
#' provider and updates the global options accordingly.
#'
#' @param provider The provider for which to set the current model.
#' @param model An optional model to override the default model for the
#'  provider, if any.
#'
#' @return Invisibly, the updated global options.
#'
#' @export
set_llmr_model <- function(
  provider,
  model = NULL
) {

  all_models <- getOption("llmr_stored_models", list())

  if (length(all_models) == 0) {
    warning("No models have been stored yet.",
            call. = FALSE, immediate. = TRUE)
    return(invisible())
  }

  if (!(provider %in% names(all_models))) {
    warning("No stored model for provider '", provider, "'.",
            call. = FALSE, immediate. = TRUE)
    return(invisible())
  }

  this_model <- all_models[[provider]]

  if (!is.null(model)) {
    this_model$model <- model
  }

  # Set current model
  options(
    llmr_model = this_model$model,
    llmr_endpoint = this_model$endpoint,
    llmr_llm_provider = this_model$api_type,
    llmr_api_key = this_model$api_key,
    llmr_api_version = this_model$api_version
  )
}

sanitize_json_output <- function(x) {
  before <- x
  
  # Remove leading and trailing whitespace
  x <- trimws(x)
  
  # Remove newlines and extra spaces outside of quoted strings
  x <- gsub("\\s+(?=([^\"]*\"[^\"]*\")*[^\"]*$)", " ", x, perl = TRUE)
  
  # Remove backslashes before quotes
  x <- gsub("\\\\(?=\")", "", x, perl = TRUE)
  
  # Add proper indentation
  x <- gsub("(\\{|\\}|,)", "\\1\n", x)
  x <- gsub("\":\"", "\": \"", x)
  x <- gsub("\\}\\s*$", "}\n", x)
  
  # Apply indentation
  lines <- strsplit(x, "\n")[[1]]
  indent <- 0
  result <- character(length(lines))
  for (i in seq_along(lines)) {
    if (grepl("\\}", lines[i])) {
      indent <- indent - 1
    }
    result[i] <- paste0(strrep("  ", indent), trimws(lines[i]))
    if (grepl("\\{", lines[i])) {
      indent <- indent + 1
    }
  }
  x <- paste(result, collapse = "\n")
  
  # Apply existing sanitization steps
  x <- gsub("\\n+", "\n", x)
  x <- gsub("\\s+", " ", x)
  x <- gsub(
    "^```(json)?\\n?", "", x,
    ignore.case = TRUE)
  x <- gsub("```\\n*$", "", x)
  
  if (before != x) {
    warning("JSON output needed sanitization!",
    call. = FALSE, immediate. = FALSE)
  }
  
  x
}