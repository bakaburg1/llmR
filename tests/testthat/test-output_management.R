test_that("prompt_llm sanitizes non-compliant JSON output", {
  # Create a sample list
  sample_list <- list(
    name = "John Doe",
    age = 30,
    hobbies = c("reading", "cycling")
  )

  # Convert the list to JSON and make it non-compliant
  non_compliant_json <- paste0(
    "```json\n",
    jsonlite::toJSON(sample_list, auto_unbox = TRUE),
    "\n```"
  )

  # Set up the mock LLM to return the non-compliant JSON
  withr::with_options(
    list(
      llmr_mock_fun_response = non_compliant_json,
      llmr_mock_fun_status = 200
    ),
    {
      # Call prompt_llm with the mock LLM and test that a warning was issued
      expect_warning(
        {
          result <- prompt_llm(
            "Generate JSON",
            provider = "mock",
            force_json = TRUE
          )
        },
        "JSON output needed sanitization!"
      )

      # Parse the result and compare with the original list
      parsed_result <- jsonlite::fromJSON(result)
      expect_equal(parsed_result, sample_list)

      # Ensure the result doesn't contain the markdown code block
      expect_false(grepl("```json", result))
      expect_false(grepl("```$", result))
    }
  )
})
