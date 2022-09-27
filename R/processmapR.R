#' @title processmapR - Process Maps in R
#'
#' @description This package provides several useful techniques process visualization.
#'
#' @docType package
#' @name processmapR
#'
#' @import dplyr
#' @import bupaR
#' @import edeaR
#' @import ggplot2
#' @import DiagrammeR
#' @import stringr
#' @import shiny
#' @import miniUI
#' @import forcats
#' @importFrom glue glue
#' @importFrom hms as.hms
#' @importFrom data.table data.table as.data.table :=
#' @importFrom purrr set_names
#' @importFrom stats median na.omit quantile sd as.formula reorder
#' @importFrom utils head setTxtProgressBar txtProgressBar data
#' @importFrom scales rescale
#' @importFrom plotly ggplotly renderPlotly plotlyOutput
#' @importFrom rlang arg_match is_integerish
#' @importFrom cli cli_abort cli_warn
#' @importFrom lifecycle deprecated

utils::globalVariables(c(".", ".order"))

#' @useDynLib processmapR, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("processmapR", libpath)
}
