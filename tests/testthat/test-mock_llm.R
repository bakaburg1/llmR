# tests/testthat/test-mock_llm.R

test_that("use_mock_llm returns expected output for valid input", {
  body <- list(messages = list(list(role = "user", content = "Hello")))
  response <- use_mock_llm(body)

  expect_s3_class(response, "response")
  expect_equal(response$status_code, 200L)
  expect_equal(response$headers$`Content-Type`, "application/json")

  content <- httr::content(response, as = "parsed", encoding = "UTF-8")
  expect_equal(content$choices[[1]]$message$content, "Test successful!")
  expect_equal(content$usage$prompt_tokens, 10)
  expect_equal(content$usage$completion_tokens, 20)
  expect_equal(content$usage$total_tokens, 30)
})

test_that("use_mock_llm uses custom response if provided", {
  body <- list(messages = list(list(role = "user", content = "Hello")))
  custom_response <- "Custom test successful!"
  response <- use_mock_llm(body, response = custom_response)

  content <- httr::content(response, as = "parsed", encoding = "UTF-8")
  expect_equal(content$choices[[1]]$message$content, custom_response)
})

test_that("use_mock_llm respects global mock response option", {
  body <- list(messages = list(list(role = "user", content = "Hello")))

  test_options <- list(
    llmr_mock_fun_response = "Global test successful!"
  )

  withr::with_options(test_options, {
    response <- use_mock_llm(body)

    content <- httr::content(response, as = "parsed", encoding = "UTF-8")
    expect_equal(
      content$choices[[1]]$message$content, "Global test successful!")

  })
})
