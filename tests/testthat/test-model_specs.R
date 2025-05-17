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

test_that("prompt_llm accepts model specification label", {
  # Record a model with parameters
  record_llmr_model(
    label = "test-model",
    provider = "custom",
    endpoint = "http://test.endpoint",
    model = "test-model",
    api_key = "test-key",
    parameters = list(
      temperature = 0.7,
      max_tokens = 1000
    )
  )

  # Store the request parameters for inspection
  request_params <- NULL
  request_url <- NULL
  request_headers <- NULL
  
  withr::with_options(
    list(llmr_log_requests = TRUE),
    {
      testthat::local_mocked_bindings(
        POST = function(url, body, add_headers, ...) {
          request_url <<- url
          request_params <<- jsonlite::fromJSON(body)
          request_headers <<- add_headers
          use_mock_llm(response = "Test response")
        },
        .package = "httr"
      )

      # Call with model specification label
      prompt_llm("Hello", model_specification = "test-model")
      expect_equal(request_url, "http://test.endpoint")
      expect_equal(request_params$temperature, 0.7)
      expect_equal(request_params$max_tokens, 1000)
      expect_equal(request_params$model, "test-model")
      expect_true(any(grepl("test-key", unlist(request_headers))))

      # Should error with invalid model specification label
      expect_error(
        prompt_llm("Hello", model_specification = "invalid-model"),
        "No stored model specification under label 'invalid-model'"
      )
    }
  )
})

test_that("prompt_llm accepts complete model specification", {
  # Store the request parameters for inspection
  request_params <- NULL
  request_url <- NULL
  request_headers <- NULL
  
  withr::with_options(
    list(llmr_log_requests = TRUE),
    {
      testthat::local_mocked_bindings(
        POST = function(url, body, add_headers, ...) {
          request_url <<- url
          request_params <<- jsonlite::fromJSON(body)
          request_headers <<- add_headers
          use_mock_llm(response = "Test response")
        },
        .package = "httr"
      )

      # Call with complete model specification
      prompt_llm(
        "Hello",
        model_specification = list(
          provider = "custom",
          endpoint = "http://custom.endpoint",
          model = "custom-model",
          api_key = "custom-key",
          parameters = list(
            temperature = 0.8,
            max_tokens = 2000
          )
        )
      )
      expect_equal(request_url, "http://custom.endpoint")
      expect_equal(request_params$temperature, 0.8)
      expect_equal(request_params$max_tokens, 2000)
      expect_equal(request_params$model, "custom-model")
      expect_true(any(grepl("custom-key", unlist(request_headers))))

      # Parameters in model specification can be overridden
      prompt_llm(
        "Hello",
        model_specification = list(
          provider = "custom",
          endpoint = "http://custom.endpoint",
          model = "custom-model",
          api_key = "custom-key",
          parameters = list(
            temperature = 0.8,
            max_tokens = 2000
          )
        ),
        params = list(temperature = 0.9)
      )
      expect_equal(request_params$temperature, 0.9)
      expect_equal(request_params$max_tokens, 2000)

      # Should error with invalid model specification type
      expect_error(
        prompt_llm("Hello", model_specification = 123),
        "'model_specification' must be either a character string \\(label\\) or a list \\(specification\\)"
      )
    }
  )
}) 