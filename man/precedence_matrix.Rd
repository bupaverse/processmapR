% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/precedence_matrix.R
\name{precedence_matrix}
\alias{precedence_matrix}
\title{Precendence Matrix}
\usage{
precedence_matrix(
  eventlog,
  type = c("absolute", "relative", "relative-antecedent", "relative-consequent",
    "relative-case")
)
}
\arguments{
\item{eventlog}{The event log object to be used}

\item{type}{The type of precedence matrix, which can be absolulte, relative, relative-antecedent or relative-consequent. Absolute will return
a matrix with absolute frequencies, relative will return global relative frequencies for all antecedent-consequent pairs.
Relative-antecedent will return relative frequencies within each antecendent, i.e. showing the relative proportion of consequents within each antecedent.
Relative-consequent will do the reverse.}
}
\description{
Construct a precendence matrix, showing how activities are followed by each other.

\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
This function is deprecated and replaced by the \code{\link{process_matrix}} function, which shared the same usage with \code{\link{process_map}}
}
\examples{
\dontrun{
library(eventdataR)
data(patients)
precedence_matrix(patients)
}
}
\keyword{internal}
