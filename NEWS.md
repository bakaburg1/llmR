# 1.1.0

## Gemini API support, better error handling and added prompt history logging

#### Enhancements
- **Add session management for LLM interactions**: Introduced `session_id` parameter to `prompt_llm` function to track sessions. Added utility functions for session management, including `set_session_id`, `get_session_id`, `get_session_data`, `get_session_data_ids`, and `remove_session_data`. Updated `prompt_llm` to store interaction data in session history (Commit: bba46ec).
- **Enhance request error handling and time logging**: Moved rate limit handling to the `is_rate_limit_exceeded` function. Introduced `stop_on_response_error` and `stop_on_no_output` for error handling. Added `format_timediff` utility function to format difftime objects for logging. Removed dependency on `tictoc` package to record processing time (Commit: 0655c318).
- **Enhance handling of incomplete answers**: Changed warnings to messages for better user feedback when an answer exhausts the context window. Store incomplete answers in the `llmr_incomplete_answers` option object instead of saving to a file, to meet CRAN requirements. Improved user interaction by asking how to proceed if in interactive mode and no choice was set in the options (Commit: 78069e0).
- **Add support for Google Gemini Language Model**: Added a new function `use_gemini_llm` to support interactions with the Google Gemini Language Model. This function sends requests to the Google Gemini API, processes the responses to be compatible with the OpenAI API, and includes error handling and logging capabilities (Commit: 72176fb).
- **Enhance mock LLM function with error simulation and response delay**: Added new parameters to the `use_mock_llm` function to simulate various response scenarios, including status code, retry after, and response delay (Commit: 1bf2a4b).

#### Fixes
- **Remove only dependency on stringr**: Replaced `stringr::str_glue` with `sprintf` to remove dependency on `stringr` (Commit: 5aad166).

#### Documentation
- **Documentation update**: Enhanced provider support and added session ID parameter documentation. Improved error message for unsupported LLM providers. Removed redundant message logging. Fixed indentation and formatting issues in the code (Commit: ec8e5d2).
- **Documentation update**: Updated the `provider` parameter documentation to include Google Gemini and custom models. Added `session_id` parameter to store conversation data for review and post-processing (Commit: 72176fb).

#### Summary
This pull request introduces significant enhancements to the `llmR` package, including session management, improved error handling, and support for the Google Gemini Language Model. It also enhances the mock LLM function to simulate various response scenarios and improves user interaction when handling incomplete answers. Additionally, it includes several documentation updates and fixes minor issues.

# llmR 1.0.1

## Setup package for CRAN submission

Small changes to the package to prepare for CRAN submission.

# llmR 1.0.0

## Enhanced LLM Prompting, Mocking, and Documentation

#### Enhancements
- Mock LLM Functionality: Introduced `use_mock_llm` function to simulate LLM calls for testing purposes, avoiding actual requests to LLM providers (Commit: f190452).

#### Fixes
- Error Handling Improvement: Improved error logging for rate limit issues and installation of suggested packages (Commits: de51621, bd275ed).
- Typo Correction: Fixed a typo in the Azure LLM function error message (Commit: e54c4a6).
- JSON Response Handling: Ensured `force_json` parameter defaults to FALSE in recursive calls to allow the completion of a previously started JSON structure (Commit: b030327).

#### Documentation
- README and Docs Overhaul: General improvements to the README and function documentation to reflect new features and usage clarity (Commit: 8a82c01).

#### Summary
This update introduces significant enhancements to the `llmR` package, including the ability to mock LLM calls for testing, support for custom LLM providers, and various fixes to improve error handling and response processing. The documentation has also been overhauled for better clarity and to showcase the new features.

# llmR 0.1.0

* Initial version.
