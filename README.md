
<!-- README.md is generated from README.Rmd. Please edit that file -->

# llmR: R Interface to Large Language Models

<!-- badges: start -->
<!-- badges: end -->

`llmR` is an R package that provides a seamless interface to various
Large Language Models (LLMs) such as OpenAI’s GPT models, Azure’s
language models, Google’s Gemini models, or custom local servers. It
simplifies the process of sending prompts to these models and handling
their responses, including rate limiting, retries, and error processing.

## Features

- **Unified API**: Setup and easily switch between different LLM
  providers and models using a consistent set of functions.
- **Prompt Processing**: Convert chat messages into a standard format
  suitable for LLMs.
- **Output Processing**: Can request JSON output from the LLMs and tries
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
# install.packages("remotes")
remotes::install_github("bakaburg1/llmR")
```

## Usage

### Setup

Before using `llmR`, you need to set up your language model providers.
You can do this using the `record_llmr_model()` function, which allows
you to store the credentials for multiple models in the session options.
Users can then use `set_llmr_model(label, model)` to activate one of the
stored models. The `model` argument is not mandatory if you want to use
a default model. For the sake of simplicity, you can for example set two
models specifications (i.e., under different labels) to use different
models from the same provider without having to remember the `model`
name.

A useful tip is to put a call of `record_llmr_model()` for each model of
interest in the user `.Rprofile` so that the models are immediately
available in the session.

These are examples of how to set up LLMs from various providers:

``` r
# In your .Rprofile file. You can edit it by using usethis::edit_r_profile()

# Set up OpenAI language model
llmR::record_llmr_model(
  # A label to identify the model specification to use with set_llmr_model()
  label = "openai",
  # The provider name which corresponds to different API structures
  provider = "openai",
  # Optional model name for endpoints with multiple models such the OpenAi one
  model = "gpt-4o",
  api_key = "your_openai_api_key")

# Set up Azure language model
llmR::record_llmr_model(
    label = "gpt-4_azure",
    provider = "azure",
    # Some model providers require the endpoint to be specified
    endpoint = "https://your-resource-name.openai.azure.com",
    model = "your-deployment-id",
    api_key = "your_azure_api_key",
    # The api version is a parameter required for Azure hosted models
    api_version = "2024-06-01")

# Set up Google Gemini language model
llmR::record_llmr_model(
    label = "gemini",
    provider = "gemini",
    model = "gemini-1.5-pro",
    api_key = "your_gemini_api_key")

# Anthropic Claude model via "openrouter" as custom provider
llmR::record_llmr_model(
    label = "openrouter",
    provider = "custom",
    endpoint =  "https://openrouter.ai/api/v1/chat/completions",
    model = "anthropic/claude-3-opus-20240229",
    api_key = "your_openrouter_api_key")

# Mistral Large model via Azure
llmR::record_llmr_model(
    label = "Mistral-large-2407",
    provider = "custom",
    endpoint = "https://Mistral-large-2407-my_azure_resource.server_location.models.ai.azure.com/v1/chat/completions",
    api_key = "your_model_api_key")
    
# Example of local model served by Ollama
llmR::record_llmr_model(
  label = "ollama-llama3.1",
  endpoint = "http://localhost:11434/v1/chat/completions",
  model = "llama3.1",
)

# Choose your default model
llmR::set_llmr_model("openai")
```

### Basic Example

Here’s a simple example of how to send a prompt to an LLM provider:

``` r
# Send a prompt to the regestered model (OpenAi gpt-4o in this case)
response <- prompt_llm(
  messages = c(user = "Hello there!")
)

cat(response) # cat() allows to get a formatted output
```

### Prompting the LLMs

The `prompt_llm` function accepts prompts in several formats:

1.  **Single message**: A single string message from the user.
2.  **Named vector**: A named vector where each element represents a
    message with a specified role.
3.  **List of messages**: A list where each element is a named vector
    with role and content.
4.  **Batch prompting**: A list of lists of messages for parallel
    processing (not fully tested for all providers).

### Logging

You can enable logging to track the time taken for each request and the
number of tokens sent and generated by setting the `llmr_log_requests`
option to `TRUE`.

### Prompt History

The `llmR` package provides functionality to store and manage the
history of prompts and responses for later review. This is achieved
through a session management system. Key functions include:

- `set_session_id()`: Sets or generates a unique session ID.
- `store_llm_session_data()`: Automatically called by `prompt_llm()` to
  store interaction data.
- `get_session_id()`: Retrieves the current session ID.
- `get_session_data()`: Retrieves stored data for a specific session or
  all sessions.
- `get_session_data_ids()`: Returns a list of all stored session IDs.
- `remove_session_data()`: Removes data for a specific session or all
  sessions.

Here’s an example of how to set a session ID, store interaction data,
and retrieve it:

``` r
# Set a unique session ID
session_id <- set_session_id()

# Send a prompt and store the interaction data
response <- prompt_llm(messages = c(user = "Hello there!"))

# Retrieve the stored session data
session_data <- get_session_data(session_id)
cat(session_data)
```

Such tools can be useful for more advanced use such builiding chatbots
or auditing your LLM usage.

By default, `prompt_llm()` automatically stores session data. You can
review and analyze your interaction history using these functions, which
is useful for debugging, improving prompts, or auditing your LLM usage.

### Custom Providers

The package implements interfaces for “OpenAI”, “Azure OpenAI GPT”, and
“Google Gemini” models, but you can also utilize other custom providers
compatible with the “OpenAI” API specification or even your own LLM
functions for providers with different API structures.

To create a custom provider function, name it with the pattern:
`use_<custom_provider>_llm`. For example, `use_myProvider_llm`. The
function should accept a `body` argument and return an `httr` response
object.

### Mock Calls

The package provides a way to simulate LLM responses for testing
purposes. You can use the `prompt_llm()` function with “mock” as the
`label` argument or by setting the `llmr_llm_provider` option to “mock”.
The `llmr_mock_response` option can be set to a custom response that
will be used by the mock functions.

## Citation

If you use `llmR` in your research, please cite it as follows:

    D'Ambrosio, A. (2024). llmR: R Interface to Large Language Models. URL: https://github.com/bakaburg1/llmR
