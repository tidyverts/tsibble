% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidyr-verbs.R
\name{unnest_tsibble}
\alias{unnest_tsibble}
\title{Unnest a data frame consisting of tsibbles to a tsibble}
\usage{
unnest_tsibble(data, cols, key = NULL, validate = TRUE)
}
\arguments{
\item{data}{A data frame contains homogenous tsibbles in the list-columns.}

\item{cols}{Names of columns to unnest.}

\item{key}{Variable(s) that uniquely determine time indices. \code{NULL} for
empty key, and \code{c()} for multiple variables. It works with tidy selector
(e.g. \code{\link[dplyr:reexports]{dplyr::starts_with()}}).}

\item{validate}{\code{TRUE} suggests to verify that each key or each combination
of key variables leads to unique time indices (i.e. a valid tsibble). If you
are sure that it's a valid input, specify \code{FALSE} to skip the checks.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
}
\keyword{internal}
