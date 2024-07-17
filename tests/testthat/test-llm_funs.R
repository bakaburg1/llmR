# tests/testthat/test-llm_funs.R

test_options <- list(
  llmr_model = "test-model",
  llmr_api_key = "test-api-key",
  llmr_azure_gpt_deployment = "test-deployment",
  llmr_azure_gpt_resource = "test-resource",
  llmr_azure_api_version = "test-api-version",
  llmr_endpoint = "http://localhost:8080"
)

set_session_id("test")
on.exit({
  remove_session_data("test")
})

withr::with_options(test_options, {
  test_that("use_openai_llm sends request and handles response", {
    with_mocked_bindings(
      POST = function(...) {use_mock_llm(response = "OpenAI LLM Response")},
      .package = "httr",
      {
        body <- list(messages = list(list(role = "user", content = "Hello")))
        result <- use_openai_llm(body)
        expect_equal(result$status_code, 200)

        prompt_llm("test", provider = "openai") |>
          expect_equal("OpenAI LLM Response")
      }
    )
  })

  test_that("use_azure_llm sends request and handles response", {
    with_mocked_bindings(
      POST = function(...) {use_mock_llm(response = "Azure LLM Response")},
      .package = "httr",
      {
        body <- list(messages = list(list(role = "user", content = "Hello")))
        result <- use_azure_llm(body)
        expect_equal(result$status_code, 200)

        prompt_llm("test", provider = "azure") |>
          expect_equal("Azure LLM Response")
      }
    )
  })

  test_that("use_custom_llm sends request and handles response", {
    with_mocked_bindings(
      POST = function(...) {use_mock_llm(response = "Custom LLM Response")},
      .package = "httr",
      {
        body <- list(messages = list(list(role = "user", content = "Hello")))
        result <- use_custom_llm(body)

        expect_equal(result$status_code, 200)

        prompt_llm("test", provider = "custom") |>
          expect_equal("Custom LLM Response")
      }
    )
  })

  # test_that("use_gemini_llm sends request and handles response", {
  #   with_mocked_bindings(
  #     # Gemini do not generate OpenAi compatible responses, so we need to build
  #     # the mock response from scratch
  #     POST = mock_httr_post(
  #       list(
  #         candidates = list(list(
  #           content = list(
  #             parts = list(list(text = "Gemini Mock Response")),
  #             role = "model"
  #           ),
  #           finishReason = "STOP"
  #         )),
  #         usageMetadata = list(
  #           promptTokenCount = 10,
  #           candidatesTokenCount = 20,
  #           totalTokenCount = 30
  #         )
  #       )), .package = "httr",
  #     {
  #       body <- list(messages = list(list(role = "user", content = "Hello")))
  #       result <- use_gemini_llm(body)
  #       expect_equal(result$status_code, 200)
  #
  #       prompt_llm("test", provider = "gemini") |>
  #         expect_equal("Gemini Mock Response")
  #     }
  #   )
  # })

  test_that("use_gemini_llm sends request and handles response", {
    # Mock successful response in Gemini API format
    successful_response <- list(
      candidates = list(list(
        content = list(
          parts = list(list(text = "Gemini Mock Response")),
          role = "model"
        ),
        finishReason = "STOP"
      )),
      promptFeedback = list(blockReason = NULL),
      usageMetadata = list(
        promptTokenCount = 10,
        candidatesTokenCount = 20,
        totalTokenCount = 30
      )
    )

    # Mock RECITATION response in Gemini API format
    recitation_response <- list(
      candidates = list(list(
        content = list(
          parts = list(list(text = "Recitation Response")),
          role = "model"
        ),
        finishReason = "RECITATION"
      )),
      promptFeedback = list(blockReason = NULL),
      usageMetadata = list(
        promptTokenCount = 5,
        candidatesTokenCount = 10,
        totalTokenCount = 15
      )
    )

    # Counter for POST calls
    call_count <- 0

    # Mock POST function
    mock_post <- function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        content <- jsonlite::toJSON(recitation_response, auto_unbox = TRUE)
      } else {
        content <- jsonlite::toJSON(successful_response, auto_unbox = TRUE)
      }

      resp <- list(
        url = "https://example.com",
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw(content)
      )
      class(resp) <- "response"
      return(resp)
    }

    with_mocked_bindings(
      POST = mock_post,
      .package = "httr",
      {
        body <- list(messages = list(list(role = "user", content = "Hello")))
        result <- use_gemini_llm(body)

        # Check that the function made multiple calls (due to RECITATION)
        expect_equal(call_count, 2)

        # Verify the response structure
        expect_equal(result$status_code, 200)

        # Parse the content
        parsed_content <- jsonlite::fromJSON(rawToChar(result$content))

        # Check the response structure
        expect_true("choices" %in% names(parsed_content))
        expect_true("message" %in% names(parsed_content$choices))
        expect_true("content" %in% names(parsed_content$choices$message))
        expect_equal(parsed_content$choices$message$content, "Gemini Mock Response")
        expect_equal(parsed_content$choices$finish_reason, "STOP")

        # Check usage information
        expect_equal(parsed_content$usage$prompt_tokens, 10)
        expect_equal(parsed_content$usage$completion_tokens, 20)
        expect_equal(parsed_content$usage$total_tokens, 30)

        # Test with prompt_llm
        response <- prompt_llm("test", provider = "gemini")
        expect_equal(response, "Gemini Mock Response")
      }
    )
  })




})

remove_session_data("test")
