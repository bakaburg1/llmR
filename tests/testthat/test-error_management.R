# tests/testthat/test-error_managent.R

## is_rate_limit_exceeded

test_that("is_rate_limit_exceeded returns TRUE when rate limit is exceeded", {
  response <- use_mock_llm(status = 429)
  expect_warning(
    failure <- is_rate_limit_exceeded(
      response,
      wait = FALSE,
      log_request = FALSE
    ),
    "Rate limit exceeded"
  )
  expect_true(failure)
})

test_that("is_rate_limit_exceeded waits for at least the specified time when rate limit is exceeded", {
  response <- use_mock_llm(status = 429, retry_after = .5)
  start_time <- Sys.time()
  expect_warning(
    failure <- is_rate_limit_exceeded(
      response,
      wait = TRUE,
      log_request = FALSE
    ),
    "Rate limit exceeded"
  )
  end_time <- Sys.time()
  expect_gte(
    as.numeric(difftime(end_time, start_time, units = "secs")),
    .5
  )
})

test_that("is_rate_limit_exceeded logs warning and error messages", {
  opts <- options()
  on.exit(options(opts))
  options(
    llmr_attempt_number = 1
  )

  max_attempts <- 3

  response <- use_mock_llm(
    status = 429,
    retry_after = 0.05,
    simul_delay = 0.05
  )

  fn <- purrr::quietly(purrr::safely(is_rate_limit_exceeded))

  iter <- 1
  prev <- getOption("llmr_attempt_number")

  while (iter <= max_attempts + 1) {
    res <- fn(
      response,
      wait = FALSE,
      log_request = TRUE,
      max_attempts = max_attempts
    )
    iter <- iter + 1

    # Test that "llmr_attempt_number" gets increased. If the maximum number of
    # attempts is reached, the value should be reset to 1.
    expect_in(getOption("llmr_attempt_number"), c(1, prev + 1))
    prev <- getOption("llmr_attempt_number")
  }

  expect_setequal(
    res$warnings,
    c(
      "Rate limit exceeded. Waiting before retrying.",
      "Mock rate limit error."
    )
  )

  expect_setequal(
    res$result$error$message,
    paste0(
      "Maximum number of attempts (",
      max_attempts,
      ") on `retry` error reached."
    )
  )
})

test_that("is_rate_limit_exceeded returns FALSE when status code is not 429", {
  response <- use_mock_llm(status = 200)

  failure <- is_rate_limit_exceeded(response, wait = FALSE, log_request = FALSE)
  expect_false(failure)
})

## stop_on_response_error

test_that("stop_on_response_error stops execution on HTTP error", {
  response <- use_mock_llm(status = 500)

  expect_error(
    stop_on_response_error(response),
    "Error in LLM request \\(500\\): Mock server error."
  )
})

test_that("stop_on_response_error stops for errors in the response content but not the HTTP code", {
  response <- use_mock_llm(
    status = 200,
    error_msg = list(message = "Mocked error message.")
  )

  expect_error(
    stop_on_response_error(response),
    "Error in LLM request: Mocked error message."
  )
})

test_that("stop_on_response_error does not stop execution if no HTTP error", {
  response <- use_mock_llm(status = 200)
  expect_silent(stop_on_response_error(response))
})

## stop_on_no_output

test_that("stop_on_no_output stops execution when no tokens were generated", {
  parsed <- list(usage = list(completion_tokens = 0))
  expect_error(stop_on_no_output(parsed), "No tokens were generated.")

  parsed <- list(usage = list(completion_tokens = NULL))
  expect_error(stop_on_no_output(parsed), "No tokens were generated.")

  parsed <- list(usage = list(completion_tokens = character(0)))
  expect_error(stop_on_no_output(parsed), "No tokens were generated.")
})

test_that("stop_on_no_output does not stop execution when tokens were generated", {
  parsed <- list(usage = list(completion_tokens = 10))
  expect_silent(stop_on_no_output(parsed))
})
