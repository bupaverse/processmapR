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
#' @importFrom stats median
#' @importFrom stats na.omit
#' @importFrom stats quantile
#' @importFrom stats sd
#' @importFrom utils head
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom utils data

globalVariables(c("case_classifier","consequent","end_case","resource_classifier","antecedent","rel_n","rel_antecedent","rel_consequent",
				  "timestamp_classifier","ts","aid","case","event","event_classifier","next_event","next_node_id",
				  "node_id","rel_antecedent","rel_consequent","cum_freq_lag","activity_instance_classifier",
				  ".N",".","absolute_frequency","as.formula","end","start","reorder","dur",
				  "start_case","start_relative","trace_id","relative_frequency","cum_freq","com_freq_lag","rank_event"))

NULL
