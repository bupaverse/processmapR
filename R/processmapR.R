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
#' @importFrom stats median
#' @importFrom stats na.omit
#' @importFrom stats quantile
#' @importFrom stats sd
#' @importFrom utils head
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom utils data

globalVariables(c("case_classifier","consequent","antecedent","rel_n","rel_antecedent","rel_consequent",
				  "timestamp_classifier","ts","aid","case","event","event_classifier","next_event","next_node_id",
				  "node_id","rel_antecedent","rel_consequent"))

NULL
