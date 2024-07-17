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
