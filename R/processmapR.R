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
#' @import ggthemes
#' @import DiagrammeR
#' @import stringr
#' @import shiny
#' @import miniUI
#' @import forcats
#' @importFrom glue glue
#' @importFrom hms as.hms
#' @importFrom data.table data.table
#' @importFrom data.table :=
#' @importFrom data.table as.data.table
#' @importFrom purrr set_names
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats median
#' @importFrom stats na.omit
#' @importFrom stats quantile
#' @importFrom stats sd
#' @importFrom stats as.formula
#' @importFrom utils head
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom utils data
#' @importFrom scales rescale
#' @importFrom plotly ggplotly
#' @importFrom plotly renderPlotly
#' @importFrom plotly plotlyOutput

globalVariables(".")
