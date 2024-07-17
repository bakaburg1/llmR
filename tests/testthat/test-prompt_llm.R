# tests/testthat/test-prompt_llm.R

set_session_id("test")
on.exit({
  remove_session_data("test")
})

test_that("prompt_llm sends messages and handles responses", {
  messages <- c(user = "Hello")
  result <- prompt_llm(messages = messages, provider = "mock")
  expect_equal(result, "Test successful!")
})

test_that("prompt_llm handles rate limiting warning", {

  opts <- options()
  on.exit(options(opts))
  options(
    llmr_mock_fun_status = 429,
    llmr_mock_fun_retry_after = 0.05,
    llmr_mock_fun_simul_delay = 0.05,
    llmr_attempt_number = 1,
    llmr_retry_attempts = 3
  )

  res <- purrr::quietly(purrr::safely(prompt_llm))("Hello", provider = "mock")

  # Test if "llmr_attempt_number" got reset on error
  expect_equal(
    getOption("llmr_attempt_number"), 1
  )
  expect_null(res$result$result)
  expect_equal(res$output, "")
  expect_setequal(res$warnings, c(
    "Rate limit exceeded. Waiting before retrying.",
    "Mock rate limit error."
  ))
  expect_setequal(
    res$result$error$message,
    "Maximum number of attempts (3) on `retry` error reached.")
})

test_that("prompt_llm handles errors in response", {

  opts <- options()
  on.exit(options(opts))
  options(
    llmr_mock_fun_status = 400
  )

  expect_error(
    prompt_llm("Test", provider = "mock"),
    "Error in LLM request: Mock general error")
})

test_that("prompt_llm handles missing provider", {
  opts <- options()
  on.exit(options(opts))
  options(
    llmr_llm_provider = NULL
  )

  expect_error(
    prompt_llm(messages = c(user = "Hello")),
    "Language model provider is not set.")

})

# Integration test: Combine process_messages and prompt_llm
test_that("process_messages and prompt_llm integration", {
  opts <- options()
  on.exit(options(opts))
  options(
    llmr_mock_fun_status = 200
  )

  raw_messages <- c(user = "Hello")
  processed_messages <- process_messages(raw_messages)
  result <- prompt_llm(messages = processed_messages, provider = "mock")
  expect_equal(result, "Test successful!")
})

remove_session_data("test")
