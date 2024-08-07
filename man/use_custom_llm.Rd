% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LLM_prompting.R
\name{use_custom_llm}
\alias{use_custom_llm}
\title{Use Custom Language Model}
\usage{
use_custom_llm(
  body,
  endpoint = getOption("llmr_endpoint"),
  model = getOption("llmr_model"),
  api_key = getOption("llmr_api_key"),
  log_request = getOption("llmr_log_requests", TRUE)
)
}
\arguments{
\item{body}{The body of the request.}

\item{endpoint}{The local endpoint for the language model service. Can be set
up globally using the \code{llmr_endpoint} option.}

\item{model}{Model identifier for the custom API, if needed (some API have
one model per endpoint, some multiple ones). Can be set up globally using
the \code{llmr_model} option.}

\item{api_key}{Optional API key for the custom language model services that
require it. Can be set up globally using the \code{llmr_api_key} option.}

\item{log_request}{A boolean to log the request time. Can be set up globally
using the \code{llmr_log_requests} option, which defaults to \code{TRUE}.}
}
\value{
The function returns the response from the local language model
endpoint.
}
\description{
Sends a request to a custom (local or remote) language model endpoint
compatible with the OpenAI API specification, using the parameters in the
\code{body} argument. The user can provide an API key if required. Users would not
typically call this function directly, but rather use the \code{prompt_llm}
function.
}
