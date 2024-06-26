% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LLM_prompting.R
\name{use_mock_llm}
\alias{use_mock_llm}
\title{Mock Language Model call for testing}
\usage{
use_mock_llm(
  body,
  model = "FakeLLama",
  response = getOption("llmr_mock_response", "Test successful!"),
  log_request = getOption("llmr_log_requests", TRUE)
)
}
\arguments{
\item{body}{The body of the request.}

\item{model}{The model identifier for the mock response. Defaults to
"FakeLLama" (which obviously doesn't exist).}

\item{response}{The response to be returned by the mock call. The response
can be set globally using the \code{llmr_mock_response} option.}

\item{log_request}{A boolean to log the request time. Can be set up globally
using the \code{llmr_log_requests} option, which defaults to TRUE.}
}
\value{
The function returns the mock response.
}
\description{
This function mocks a call to a language model provider and returns a
predefined response. It is used for testing purposes to avoid making actual
requests to the language model provider. The function simulates a delay in
the response to mimic the actual response time.
}
\examples{

\dontrun{
response <- use_mock_llm(
body = list(messages = list(list(role = "user", content = "Hello"))),
model = "FakeLLama",
response = "Test successful!"
)

print(response)

}

}
