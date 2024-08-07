% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Utils.R
\name{remove_session_data}
\alias{remove_session_data}
\title{Reset LLM session data}
\usage{
remove_session_data(id = NULL)
}
\arguments{
\item{id}{The session ID to reset the data for. If not provided, all session
data will be reset.}
}
\description{
Reset the session data stored in the session option. If a session ID is
provided, only the data for that session will be reset.
}
