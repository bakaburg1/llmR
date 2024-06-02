# tests/testthat/test-llm_funs.R

test_options <- list(
  llmr_openai_model_gpt = "test-model",
  llmr_openai_api_key = "test-api-key",
  llmr_azure_deployment_gpt = "test-deployment",
  llmr_azure_resource_gpt = "test-resource",
  llmr_azure_api_key_gpt = "test-api-key",
  llmr_azure_api_version = "test-api-version",
  llmr_custom_endpoint_gpt = "http://localhost:8080",
  llmr_custom_model_gpt = "test-custom-model",
  llmr_custom_api_key = "test-custom-api-key"
)

withr::with_options(test_options, {
  test_that("use_openai_llm sends request and handles response", {
    with_mocked_bindings(
      POST = mock_httr_post(list(choices = list(list(message = list(content = "OpenAI Response"))))),
      .package = "httr",
      {
        body <- list(messages = list(list(role = "user", content = "Hello")))
        result <- use_openai_llm(body)
        expect_equal(result$status_code, 200)
      }
    )
  })

  test_that("use_azure_llm sends request and handles response", {
    with_mocked_bindings(
      POST = mock_httr_post(list(choices = list(list(message = list(content = "Azure Response"))))),
      .package = "httr",
      {
        body <- list(messages = list(list(role = "user", content = "Hello")))
        result <- use_azure_llm(body)
        expect_equal(result$status_code, 200)
      }
    )
  })

  test_that("use_custom_llm sends request and handles response", {
    with_mocked_bindings(
      POST = mock_httr_post(list(choices = list(list(message = list(content = "Custom LLM Response"))))),
      .package = "httr",
      {
        body <- list(messages = list(list(role = "user", content = "Hello")))
        result <- use_custom_llm(body)
        expect_equal(result$status_code, 200)
      }
    )
  })
})
