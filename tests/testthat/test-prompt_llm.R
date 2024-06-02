# tests/testthat/test-prompt_llm.R

test_that("prompt_llm sends messages and handles response", {
  with_mocked_bindings(
    use_openai_llm = mock_llm_success,
    {
      messages <- c(user = "Hello")
      result <- prompt_llm(messages = messages, provider = "openai")
      expect_equal(result, "Response")
    }
  )
})

test_that("prompt_llm handles rate limiting", {
  with_mocked_bindings(
    use_openai_llm = mock_llm_rate_limit,
    {
      with_mocked_bindings(
        use_openai_llm = mock_llm_success,
        {
          messages <- c(user = "Hello")
          result <- prompt_llm(messages = messages, provider = "openai")
          expect_equal(result, "Response")
        }
      )
    }
  )
})

test_that("prompt_llm handles rate limiting warning", {

  with_mocked_bindings(
    use_openai_llm = mock_llm_rate_limit,
    {
      messages <- c(user = "Hello")

      triggered <- FALSE

      tryCatch(
        prompt_llm(messages = messages, provider = "openai"),
        warning = function(w) {

          triggered <<- w$message == "Rate limit exceeded. Waiting before retrying."
        }
      )

      expect_true(triggered, label = "Rate limit warning triggered")
    }
  )
})

test_that("prompt_llm handles errors in response", {
  with_mocked_bindings(
    use_openai_llm = mock_llm_error,
    {
      messages <- c(user = "Hello")
      expect_error(
        prompt_llm(messages = messages, provider = "openai"),
        "Error in LLM request: Error message")
    }
  )
})

test_that("prompt_llm handles missing provider", {
  expect_error(
    prompt_llm(messages = c(user = "Hello")),
    "Language model provider is not set.")
})

# Integration test: Combine process_messages and prompt_llm
test_that("process_messages and prompt_llm integration", {
  with_mocked_bindings(
    use_openai_llm = mock_llm_success,
    {
      raw_messages <- c(user = "Hello")
      processed_messages <- process_messages(raw_messages)
      result <- prompt_llm(messages = processed_messages, provider = "openai")
      expect_equal(result, "Response")
    }
  )
})
