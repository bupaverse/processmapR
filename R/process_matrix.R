


#' Create process matrix
#'
#' @param type A process matrix type, which can be created with the functions frequency, performance and custom. The first type focusses on the frequency aspect of a process, while the second one focussed on processing time. The third one allows custom attributes to be used.
#' @param ... Other arguments
#' @inheritParams dotted_chart
#' @export process_matrix
#'

process_matrix <- function(log, type , ..., eventlog = deprecated()) {
	UseMethod("process_matrix")
}

#' @describeIn process_matrix Process matrix for event log
#' @export
process_matrix.eventlog <- function(log,
									type = frequency(),
									..., eventlog = deprecated()) {


	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn("0.4.0",
								  "process_matrix(eventlog)",
								  "process_matrix(log)")
		log <- eventlog
	}

	node_id.y <- NULL
	node_id.x <- NULL
	sec_label <- NULL
	ACTIVITY_CLASSIFIER_ <- NULL
	CASE_CLASSIFIER_ <- NULL
	TIMESTAMP_CLASSIFIER_ <- NULL
	ACTIVITY_INSTANCE_CLASSIFIER_ <- NULL
	start_time <- NULL
	min_order <- NULL
	end_time <- NULL
	n.x <- NULL
	n.y <- NULL


	base_precedence <- create_base_precedence(log, type, type)

	extra_data <- list()
	extra_data$n_cases <- n_cases(log)
	extra_data$n_activity_instances <- n_activity_instances(log)


	edges <- attr(type, "create_edges")(base_precedence, type, extra_data) %>%
		attr(type, "transform_for_matrix")(type, extra_data)

	class(edges) <- c("process_matrix", class(edges))
	attr(edges, "matrix_type") <- type


	return(edges)

}


#' @describeIn process_matrix Process matrix for activity log
#' @export
process_matrix.activitylog <- function(log,
									type = frequency(),
									..., eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn("0.4.0",
								  "process_matrix(eventlog)",
								  "process_matrix(log)")
		log <- eventlog
	}


	log %>% bupaR::to_eventlog() %>% process_matrix(type)


}
