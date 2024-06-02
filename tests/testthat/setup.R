create_mock_response <- function(status_code, content, headers = list("Content-Type" = "application/json"), url = "http://example.com") {
  response <- list(
    url = url,
    status_code = as.integer(status_code),  # Ensure status_code is integer
    headers = headers,
    content = charToRaw(
      jsonlite::toJSON(
        content, auto_unbox = TRUE))  # Encode content as raw vector
  )

  class(response) <- "response"

  response
}

mock_llm_success <- function(body, ...) {
  create_mock_response(200, list(
    choices = list(list(
      index = 0,
      message = list(role = "assistant", content = "Response"),
      logprobs = NULL,
      finish_reason = "stop"
    )),
    usage = list(
      prompt_tokens = 10,
      completion_tokens = 20,
      total_tokens = 30
    )
  ))
}

mock_llm_rate_limit <- function(body, ...) {
  create_mock_response(429, list(), list(`retry-after` = "1"))
}

mock_llm_error <- function(body, ...) {
  create_mock_response(400, list(error = list(message = "Error message")))
}

mock_httr_post <- function(content) {
  function(...) {
    response <- create_mock_response(200, content)
    class(response) <- "response"
    response
  }
}
