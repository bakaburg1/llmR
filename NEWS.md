# llmR 1.4.0

## Enhance model management: default parameters & specification labels in prompt_llm

#### Enhancements
- Support per-model default parameters in record_llmr_model(): add a new `parameters` field so users can define defaults (e.g. `temperature`, `max_tokens`) that automatically merge into `prompt_llm()` calls (Commit: b964982593bf15b26f8458c75cb65769adb108e8).
- Add `model_specification` argument to prompt_llm(): accept either a registered model label or a full specification list, overriding the global model (Commit: d212ce8bfdff511e3adb88aa73113a2dadce9d88).
- Merge stored model defaults and explicit `params` in prompt_llm(): stored defaults are combined with, and can be overridden by, call-level parameters; Azure deployments and endpoints are now mapped correctly (Commit: d212ce8bfdff511e3adb88aa73113a2dadce9d88).
- Remove implicit default temperature: `prompt_llm()` no longer injects `temperature = 0` by default; defaults must come from model specs or explicit `params` (Commit: 773e2fe66e387082f2f699ac7daff1bf0544d15e).
- Comprehensive tests for model specification handling: new `test-model_specs.R` suite validates both label-based and full list specifications, error cases, and parameter overrides (Commit: f89714fee2a48a415c8c7fab514c13509aeddb9e).

#### Fixes
- Include HTTP status codes in LLM error messages and warnings for clarity, and reset retry counters correctly on rate-limit errors (Commits: dfc1e4802d733f620376e3b430e64a27a467df8d, 56c5ec7c7353e7e32cc9ce5950dc91ef4ac4b890).
- Correct documentation paths (R/utils.R → R/Utils.R) and fix minor doc typos in multiple man pages (Commit: 5dd83b175d7e7b87d544fe7eff5d828df8ffb754).
- Tighten argument filtering when dispatching to provider functions, with warnings for any unexpected fields.

#### Documentation
- Update man-pages to document `model_specification`, `parameters`, and new examples in `prompt_llm.Rd`, `record_llmr_model.Rd`, `get_llmr_model.Rd` (Commit: 5dd83b175d7e7b87d544fe7eff5d828df8ffb754).
- Revise README and NEWS for v1.3.0: describe new model management workflow, update installation snippet (`remotes::install_github()`), and add examples for recording, setting, and using models (Commits: c982d0b268c03dbcbd041ec039b77618acda3e89, 7ea15fe71a501b0e85a57f8ad398019e0549881f).
- Export new functions in NAMESPACE (`get_llmr_model`, `set_llmr_model`) and align with renamed files (Commit: 838167ce22b24f4bec481ff3f9738286a942ddc8).

#### Summary
This patch makes LLM model management far more flexible: you can now record per-model defaults, pass a label or full spec to `prompt_llm()`, and override parameters cleanly. A slew of tests, doc updates, and build fixes round out the release.

# llmR 1.3.0

## llmR: Enhanced Model Management, JSON Sanitization, and Documentation Updates

#### Enhancements
- **Enhanced Model Management Functions**: Introduced `record_llmr_model` to store LLM model specifications and `set_llmr_model` to set the current LLM model for subsequent operations. This improves flexibility in managing multiple LLM providers and configurations (Commit: [53e4028](https://github.com/bakaburg1/llmR/commit/53e40285a98585d9a04a809c258d7f2f7525e34a)).
- **JSON Sanitization for LLM Responses**: Added `sanitize_json_output` function to clean and format JSON output from LLM responses, ensuring well-formed JSON data (Commit: [39bb754](https://github.com/bakaburg1/llmR/commit/39bb7542ab9676cfc73c33b9b933c8d2c0d23d91)).
- **Improved Session ID Handling**: Updated `session_id` parameter in `prompt_llm` to use a new session ID if not set globally, enhancing session management (Commit: [09caede](https://github.com/bakaburg1/llmR/commit/09caede6d5017c82a714e7da53a3e7db6accf7d3)).
- **Model Option Fallback**: Modified `model` parameter in `prompt_llm` to fallback to the global `llmr_model` option if not provided, ensuring smoother operation (Commit: [09caede](https://github.com/bakaburg1/llmR/commit/09caede6d5017c82a714e7da53a3e7db6accf7d3)).

#### Fixes
- **Improved Warning Message for Missing Session ID**: Enhanced the warning message in `remove_session_data` to include the session ID when it is not present in the session history, providing more context (Commit: [56c5ec7](https://github.com/bakaburg1/llmR/commit/56c5ec7c7353e7e32cc9ce5950dc91ef4ac4b890)).
- **Error Message Handling in Tests**: Updated test cases to correctly handle and match error message formats, ensuring more accurate test validation (Commits: [563029b](https://github.com/bakaburg1/llmR/commit/563029bc6409251d51d86342e3d49248077bb936), [e104fea](https://github.com/bakaburg1/llmR/commit/e104fea019462efb25eaa0d42f5414846f4a2ce4)).

#### Documentation
- **Enhanced Documentation for Model Management Functions**: Improved documentation for `record_llmr_model`, `set_llmr_model`, and `get_llmr_model` functions, providing detailed descriptions and examples (Commits: [53e4028](https://github.com/bakaburg1/llmR/commit/53e40285a98585d9a04a809c258d7f2f7525e34a), [277a36b](https://github.com/bakaburg1/llmR/commit/277a36b0bf9028a3e59f6c5024995793ee459bfd), [1345d13](https://github.com/bakaburg1/llmR/commit/1345d13976ee401cd0b3c50034f10c4e06b1c65d)).
- **Updated README with Latest Features**: Added references to new features, corrected typos, and provided additional examples for setting up various LLM providers (Commits: [7a14e81](https://github.com/bakaburg1/llmR/commit/7a14e814ceb538c800aa990580182c0cc50cf570), [696ad03](https://github.com/bakaburg1/llmR/commit/696ad03e7297ca30f383d9e65be733498aca6027)).

#### Summary
This pull request introduces significant enhancements to model management functions, including the ability to store and set LLM model specifications. It also adds JSON sanitization for LLM responses, improving the robustness of JSON handling. Additionally, session ID handling and model option fallback have been improved for smoother operation. The documentation has been updated to reflect these changes, providing detailed descriptions and examples.

# llmR 1.2.0

## Enhanced Model Management and Documentation Updates

#### Enhancements
- **Enhanced Model Management Functions**: Introduced `record_llmr_model` to store LLM model specifications and `set_llmr_model` to set the current LLM model for subsequent operations. This improves flexibility in managing multiple LLM providers and configurations (Commit: [53e4028](https://github.com/bakaburg1/llmR/commit/53e40285a98585d9a04a809c258d7f2f7525e34a)).
- **JSON Sanitization for LLM Responses**: Added `sanitize_json_output` function to clean and format JSON output from LLM responses, ensuring well-formed JSON data (Commit: [39bb754](https://github.com/bakaburg1/llmR/commit/39bb7542ab9676cfc73c33b9b933c8d2c0d23d91)).
- **Improved Session ID Handling**: Updated `session_id` parameter in `prompt_llm` to use a new session ID if not set globally, enhancing session management (Commit: [09caede](https://github.com/bakaburg1/llmR/commit/09caede6d5017c82a714e7da53a3e7db6accf7d3)).
- **Model Option Fallback**: Modified `model` parameter in `prompt_llm` to fallback to the global `llmr_model` option if not provided, ensuring smoother operation (Commit: [09caede](https://github.com/bakaburg1/llmR/commit/09caede6d5017c82a714e7da53a3e7db6accf7d3)).

#### Fixes
- **Improved Warning Message for Missing Session ID**: Enhanced the warning message in `remove_session_data` to include the session ID when it is not present in the session history, providing more context (Commit: [56c5ec7](https://github.com/bakaburg1/llmR/commit/56c5ec7c7353e7e32cc9ce5950dc91ef4ac4b890)).
- **Error Message Handling in Tests**: Updated test cases to correctly handle and match error message formats, ensuring more accurate test validation (Commits: [563029b](https://github.com/bakaburg1/llmR/commit/563029bc6409251d51d86342e3d49248077bb936), [e104fea](https://github.com/bakaburg1/llmR/commit/e104fea019462efb25eaa0d42f5414846f4a2ce4)).

#### Documentation
- **Enhanced Documentation for Model Management Functions**: Improved documentation for `record_llmr_model`, `set_llmr_model`, and `get_llmr_model` functions, providing detailed descriptions and examples (Commits: [53e4028](https://github.com/bakaburg1/llmR/commit/53e40285a98585d9a04a809c258d7f2f7525e34a), [277a36b](https://github.com/bakaburg1/llmR/commit/277a36b0bf9028a3e59f6c5024995793ee459bfd), [1345d13](https://github.com/bakaburg1/llmR/commit/1345d13976ee401cd0b3c50034f10c4e06b1c65d)).
- **Updated README with Latest Features**: Added references to new features, corrected typos, and provided additional examples for setting up various LLM providers (Commits: [7a14e81](https://github.com/bakaburg1/llmR/commit/7a14e814ceb538c800aa990580182c0cc50cf570), [696ad03](https://github.com/bakaburg1/llmR/commit/696ad03e7297ca30f383d9e65be733498aca6027)).

#### Summary
This pull request introduces significant enhancements to model management functions, including the ability to store and set LLM model specifications. It also adds JSON sanitization for LLM responses, improving the robustness of JSON handling. Additionally, session ID handling and model option fallback have been improved for smoother operation. The documentation has been updated to reflect these changes, providing detailed descriptions and examples.

# llmR 1.1.0

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
