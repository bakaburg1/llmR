
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

- **Unified API**: Interact with different LLM providers using a
  consistent set of functions.
- **Prompt Processing**: Convert chat messages into a standard format
  suitable for LLMs.
- **Error Handling**: Automatically handle errors and retry requests
  when rate limits are exceeded.
- **Custom Providers**: Extend functionality to custom LLM servers by
  writing provider-specific functions.
- **Logging**: Option to log request times for performance monitoring.

## Installation

You can install the development version of `llmR` from
[GitHub](https://github.com/bakaburg1/llmR) with:

``` r
# install.packages("pkgload")
devtools::install_github("bakaburg1/llmR")
```

## Usage

Before using `llmR`, you need to set up the necessary API keys and model
identifiers for the LLM providers you intend to use. These can be set
globally using R options.

### Basic Example

Here’s a simple example of how to send a prompt to an LLM provider:

``` r
library(llmR)

# Set your API keys and model identifiers
options(llmr_openai_api_key = "your-openai-api-key")
options(llmr_openai_model_gpt = "gpt-4o")

# Send a prompt to the OpenAI language model
response <- prompt_llm(
  messages = c(user = "Hello there!"),
  provider = "openai"
)

print(response)
```

### Custom Providers

The package implements interfaces for OpenAI, Azure, and custom LLM
providers, but you can also utilize your own LLM functions by naming
them with the following format: `use_{your provider name}_llm()`. Then
you can then pass {your provider name} as the `provider` argument in
`prompt_llm()` or set the “llmr_llm_provider” option.

## Citation

If you use `llmR` in your research, please cite it as follows:

    D'Ambrosio, A. (Year). llmR: R Interface to Large Language Models. URL: https://github.com/bakaburg1/llmR
