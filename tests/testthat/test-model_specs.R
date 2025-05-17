# tests/testthat/test-model_specs.R

# Helper functions ---
helper_setup_test_env <- function() {
  opts <- options()
  set_session_id("test")
  on.exit(options(opts))
}

helper_setup_test_env()

test_that("record_llmr_model stores model specifications with parameters", {
  # Record a model with parameters
  record_llmr_model(
    label = "test-model",
    provider = "custom",  # Using custom instead of openai as it's more flexible
    endpoint = "http://test.endpoint",
    model = "gpt-4",
    api_key = "test-key",
    parameters = list(
      temperature = 0.7,
      max_tokens = 1000
    )
  )

  # Get the stored model
  stored_models <- getOption("llmr_stored_models")
  expect_true("test-model" %in% names(stored_models))
  
  model_spec <- stored_models[["test-model"]]
  expect_equal(model_spec$provider, "custom")
  expect_equal(model_spec$model, "gpt-4")
  expect_equal(model_spec$api_key, "test-key")
  expect_equal(model_spec$parameters$temperature, 0.7)
  expect_equal(model_spec$parameters$max_tokens, 1000)
})

test_that("get_llmr_model returns model specifications including parameters", {
  # Record a model with parameters
  record_llmr_model(
    label = "test-model",
    provider = "custom",
    endpoint = "http://test.endpoint",
    model = "gpt-4",
    api_key = "test-key",
    parameters = list(
      temperature = 0.7,
      max_tokens = 1000
    )
  )

  # Set as current model
  set_llmr_model("test-model")

  # Get current model
  model_spec <- get_llmr_model()
  expect_equal(model_spec[[1]]$parameters$temperature, 0.7)
  expect_equal(model_spec[[1]]$parameters$max_tokens, 1000)

  # Get specific model
  model_spec <- get_llmr_model("test-model")
  expect_equal(model_spec[[1]]$parameters$temperature, 0.7)
  expect_equal(model_spec[[1]]$parameters$max_tokens, 1000)
})

test_that("prompt_llm uses stored parameters as defaults", {
  # Record a model with parameters
  record_llmr_model(
    label = "test-model",
    provider = "custom",
    endpoint = "http://test.endpoint",
    model = "test-model",
    parameters = list(
      temperature = 0.7,
      max_tokens = 1000
    )
  )

  # Set as current model
  set_llmr_model("test-model")

  # Store the request parameters for inspection
  request_params <- NULL
  
  withr::with_options(
    list(llmr_log_requests = TRUE),
    {
      testthat::local_mocked_bindings(
        POST = function(url, body, ...) {
          request_params <<- jsonlite::fromJSON(body)
          use_mock_llm(response = "Test response")
        },
        .package = "httr"
      )

      # Call without parameters - should use defaults
      prompt_llm("Hello")
      expect_equal(request_params$temperature, 0.7)
      expect_equal(request_params$max_tokens, 1000)

      # Call with overriding parameters
      prompt_llm("Hello", params = list(temperature = 0.9))
      expect_equal(request_params$temperature, 0.9)
      expect_equal(request_params$max_tokens, 1000)

      # Call with new parameters
      prompt_llm("Hello", params = list(top_p = 0.95))
      expect_equal(request_params$temperature, 0.7)
      expect_equal(request_params$max_tokens, 1000)
      expect_equal(request_params$top_p, 0.95)
    }
  )
})

test_that("prompt_llm logs parameters correctly", {
  # Record a model with parameters
  record_llmr_model(
    label = "test-model",
    provider = "custom",
    endpoint = "http://test.endpoint",
    model = "test-model",
    parameters = list(
      temperature = 0.7,
      max_tokens = 1000
    )
  )

  # Set as current model
  set_llmr_model("test-model")
  
  withr::with_options(
    list(llmr_log_requests = TRUE),
    {
      # Mock POST to return test response
      testthat::local_mocked_bindings(
        POST = function(url, body, ...) {
          use_mock_llm(response = "Test response")
        },
        .package = "httr"
      )

      # Test default parameters are logged
      expect_message(
        prompt_llm("Hello"),
        '"temperature":0.7.*"max_tokens":1000'
      )
      
      # Test overridden parameters are logged
      expect_message(
        prompt_llm("Hello", params = list(temperature = 0.9)),
        '"temperature":0.9.*"max_tokens":1000'
      )
      
      # Test new parameters are logged while keeping defaults
      expect_message(
        prompt_llm("Hello", params = list(top_p = 0.95)),
        '"temperature":0.7.*"max_tokens":1000.*"top_p":0.95'
      )
    }
  )
})

test_that("prompt_llm works without stored parameters", {
  # Record a model without parameters
  record_llmr_model(
    label = "test-model",
    provider = "custom",
    endpoint = "http://test.endpoint",
    model = "test-model"
  )

  # Set as current model
  set_llmr_model("test-model")

  # Mock HTTP requests
  testthat::local_mocked_bindings(
    POST = function(...) {
      use_mock_llm(response = "Test response")
    },
    .package = "httr"
  )

  # Should work with empty parameters
  expect_no_error(prompt_llm("Hello"))

  # Should work with provided parameters
  expect_no_error(prompt_llm("Hello", params = list(temperature = 0.7)))
})

test_that("prompt_llm works without current model", {
  # Clear current model
  options(llmr_current_model = NULL)

  # Mock HTTP requests
  testthat::local_mocked_bindings(
    POST = function(...) {
      use_mock_llm(response = "Test response")
    },
    .package = "httr"
  )

  # Should work with provider and parameters
  expect_no_error(
    prompt_llm("Hello", 
      provider = "custom",
      params = list(temperature = 0.7)
    )
  )
}) 