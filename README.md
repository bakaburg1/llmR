
<!-- README.md is generated from README.Rmd. Please edit that file -->

# llmR: R Interface to Large Language Models

<!-- badges: start -->
<!-- badges: end -->

`llmR` is an R package that provides a seamless interface to various
Large Language Models (LLMs) such as OpenAI’s GPT models, Azure’s
language models, or custom local servers. It simplifies the process of
sending prompts to these models and handling their responses, including
rate limiting, retries, and error processing.

## Features

- **Unified API**: Setup and easily switch between different LLM
  providers and models using a consistent set of functions.
- **Prompt Processing**: Convert chat messages into a standard format
  suitable for LLMs.
- **Ourput Processing**: Can request JSON output from the LLMs and tries
  to sanitize the response if the parsing fails.
- **Error Handling**: Automatically handle errors and retry requests
  when rate limits are exceeded. If a response is cut due to token
  limits, the package will ask the LLM to complete the response.
- **Custom Providers**: Interrogate custom endpoints (local and online)
  and allow implementation of ad-hoc LLM connection functions.
- **Mock Calls**: Allows simulation of LLM interactions for testing
  purposes.
- **Logging**: Option to log the LLM response details for performance
  and cost monitoring.

## Installation

You can install the development version of `llmR` from
[GitHub](https://github.com/bakaburg1/llmR) with:

``` r
# install.packages("pkgload")
devtools::install_github("bakaburg1/llmR")
```

## Usage

### Setup

Before using `llmR`, you need to set up your language model providers.
You can either do this via the following llmR options:

- “*llmr_llm_provider*”: set to “openai”, “azure”, “gemini” to use
  models provided by “OpenAI”, “Azure OpenAI” or “Google Gemini”
  respectively (they have different API specifications); otherwise, set
  to “custom” to use a local or remote LLMs compatible with the “OpenAI”
  API specification;
- “*llmr_endpoint*”: URL of the model endpoint for the “custom”
  provider; use the model “resource” URL for “Azure OpenAI” models;
- “*llmr_model*”: model identifier; not all providers need to specify
  this; use the “deploymentID” for “Azure OpenAI” models;
- “*llmr_api_key*”: API key for the providers that require one.
- “*llmr_api_version*”: required for “Azure OpenAI” models.

Since setting and changing these info can be cumbersome, the package
exposes the `record_llmr_model()` function which allow to store the
credentials for multiple models in the session options. The first
argument “provider” is the reference to the stored specification

Users can then use `set_llmr_model(provider, model)` to activate one of
the stored models. The second argument “model” is optional and allow to
use a different model from the one stored in the provider.

A useful tip is to call `record_llmr_model()` in the user `.Rprofile` so
that the models are immediately available in the session.

These are examples of how to set up LLMs from various providers:

``` r
# In your .Rprofile file. You can edit it by using usethis::edit_r_profile()

# Set up OpenAI language model
llmR::record_llmr_model(
  provider = "openai",
  api_type = "openai",
  model = "gpt-4o", # Default model, can be changed with set_llmr_model()
  api_key = "your_openai_api_key")

# Set up Azure language model
llmR::record_llmr_model(
    provider = "gpt-4o_azure",
    api_type = "azure",
    endpoint = "ecdctestazureopenai", # the model resource URL
    model = "gpt-4o", # the model deployment ID
    api_key = "your_azure_api_key",
    api_version = "2024-06-01")
    
# Set up Google Gemini language model
llmR::record_llmr_model(
    provider = "gemini",
    api_type = "gemini",
    model = "gemini-1.5-flash",
    api_key = "your_gemini_api_key")
    
# Anthropic Claude model via "openrouter" as custom provider
# apy_type is "custom" by default
llmR::record_llmr_model(
    provider = "openrouter",
    endpoint =  "https://openrouter.ai/api/v1/chat/completions",
    model = "anthropic/claude-3.5-sonnet",
    api_key = "your_openrouter_api_key")
    
# Mistral Large model via "Azure". The "model" is not required as this
# endpoint has only one model. api_type is "custom" by default.
llmR::record_llmr_model(
    provider = "Mistral-large-2407",
    endpoint = "https://Mistral-large-2407-my_azure_resource.server_location.models.ai.azure.com/v1/chat/completions",
    api_key = "your_model_api_key")
    
# Choose your default model
llmR::set_llmr_model("openai")
```

### Basic Example

Here’s a simple example of how to send a prompt to an LLM provider:

``` r

# Send a prompt to the OpenAI language model
response <- prompt_llm(
  messages = c(user = "Hello there!"),
  provider = "openai"
)

cat(response) # cat() allows to get a formatted output
```

### Prompting the LLMs

The `prompt_llm` function accept prompts in a number of formats:

1.  **Single message**: A single string message from the user.

``` r
message <- "Hello!"
```

2.  **Named vector**: A named vector where each element represents a
    message with a specified role.

``` r
messages <- c(system = "You are a helpful assistant.", user = "Hello!")
```

3.  **List of messages**: A list where each element is a named vector
    with role and content.

``` r
messages <- list(
  c(role = "system", content = "You are a helpful assistant."),
  c(role = "user", content = "Hello!")
)
```

4.  **Batch prompting**: This approach allows to send batches of prompts
    to process in parallel. Not all providers support this functionality
    and it’s not fully tested yet.

``` r
messages <- list(
  list(
    list(role = "system", content = "You are a helpful assistant."),
    list(role = "user", content = "Hello!")
  ),
  list(
    list(role = "system", content = "You are a helpful assistant."),
    list(role = "user", content = "How are you?")
  )
)
```

Internally, the `prompt_llm()` uses the `process_messages()` function to
convert the input messages into a standard format (the “List of
messages” one) suitable for the LLM APIs.

### Logging

You can enable logging to track the time taken for each request and the
number of tokens sent and generated by setting the `llmr_log_requests`
option to `TRUE`.

### Custom Providers

The package implements interfaces for “OpenAI”, “Azure OpenAi GPT” and
“Google Gemini” models, but you can also utilize other custom providers
compatible with the “OpenAI” API specification or even your own LLM
functions for providers with different API structures.

To create a custom provider function, you just need to name it with the
following pattern: `use_<custom provide>_llm`. For example
`use_myProvider_llm`. The function should accept a `body` argument and
return an `httr` response object; see the `use_gemini_llm()` function
body for an example on how to convert input and outputs in the required
format.

To use this function you can:

- set `provider = "myProvider"` in the `prompt_llm()` function;
- store the provider specification using `record_llmr_model()`, using
  with “myProvider” as the “api_type” option.
- set the `llmr_llm_provider` option to “myProvider”;

### Mock Calls

The package provides a way to simulate LLM responses for testing
purposes. You can use the `prompt_llm()` function with “mock” as the
`provider` argument to use the mock response or by setting the
`llmr_llm_provider` option to “mock”. The `llmr_mock_response` option
can be set to a custom response that will be used by the mock functions.
For example:

``` r
options(llmr_mock_response = "My test!")

prompt_llm("Hello", provider = "mock")
```

Will return “My test!”.

This feature is useful for unit tests of packages using `llmR`
functions. You can also use the `use_mock_llm()` function directly to
simulate a mock `httr` response object. See the function help to learn
how to simulate various responses and errors.

## Citation

If you use `llmR` in your research, please cite it as follows:

    D'Ambrosio, A. (2024). llmR: R Interface to Large Language Models. URL: https://github.com/bakaburg1/llmR
