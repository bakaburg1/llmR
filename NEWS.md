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
