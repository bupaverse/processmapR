% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resource_matrix.R
\name{resource_matrix}
\alias{resource_matrix}
\alias{resource_matrix.eventlog}
\alias{resource_matrix.activitylog}
\title{Resource Matrix}
\usage{
resource_matrix(log, type, eventlog = deprecated())

\method{resource_matrix}{eventlog}(
  log,
  type = c("absolute", "relative", "relative-antecedent", "relative-consequent"),
  eventlog = deprecated()
)

\method{resource_matrix}{activitylog}(
  log,
  type = c("absolute", "relative", "relative-antecedent", "relative-consequent"),
  eventlog = deprecated()
)
}
\arguments{
\item{log}{\code{\link[bupaR:log]{log}}: Object of class \code{\link[bupaR:log]{log}} or derivatives (\code{\link[bupaR:grouped_log]{grouped_log}},
\code{\link[bupaR:eventlog]{eventlog}}, \code{\link[bupaR:activitylog]{activitylog}}, etc.).}

\item{type}{The type of resource matrix, which can be absolulte, relative, relative_antecedent or relative_consequent. Absolute will return
a matrix with absolute frequencies, relative will return global relative frequencies for all antecedent-consequent pairs.
Relative_antecedent will return relative frequencies within each antecendent, i.e. showing the relative proportion of consequents within each antecedent. Relative_consequent will do the reverse.}

\item{eventlog}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}; please use \code{log} instead.}
}
\description{
Construct a resource matrix, showing how work is handed over
}
\section{Methods (by class)}{
\itemize{
\item \code{resource_matrix(eventlog)}: Resource matrix of event log

\item \code{resource_matrix(activitylog)}: Resource matrix of activity log

}}
\examples{
\dontrun{
library(eventdataR)
data(patients)
precedence_matrix(patients)
}

}
