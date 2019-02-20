


#' Create process matrix
#'
#' @param eventlog The event log object for which to create a process matrix
#' @param type A process matrix type, which can be created with the functions frequency, performance and custom. The first type focusses on the frequency aspect of a process, while the second one focussed on processing time. The third one allows custom attributes to be used.
#' @param ... Other arguments
#'
#' @export process_matrix
#'

process_matrix <- function(eventlog, type , ...) {
	UseMethod("process_matrix")
}

#' @describeIn process_matrix Process matrix for event log
#' @export
process_matrix.eventlog <- function(eventlog,
									type = frequency(),
									...) {
	eventlog <- ungroup_eventlog(eventlog)

	eventlog %>%
		as.data.frame() %>%
		droplevels %>%
		select(ACTIVITY_CLASSIFIER_ = !!activity_id_(eventlog),
			   ACTIVITY_INSTANCE_CLASSIFIER_ = !!activity_instance_id_(eventlog),
			   CASE_CLASSIFIER_ = !!case_id_(eventlog),
			   TIMESTAMP_CLASSIFIER_ = !!timestamp_(eventlog),
			   .order,
			   everything()) %>%
		group_by(ACTIVITY_CLASSIFIER_, ACTIVITY_INSTANCE_CLASSIFIER_, CASE_CLASSIFIER_) -> grouped_log

	perspective <- attr(type, "perspective")

	if (perspective == "custom") {
		attribute <- sym(attr(type, "attribute"))
		grouped_log %>% summarize(start_time = min(TIMESTAMP_CLASSIFIER_),
								  end_time = max(TIMESTAMP_CLASSIFIER_),
								  min_order = min(.order),
								  !!attribute := first(!!attribute)) -> base_log
	} else {
		grouped_log %>% summarize(start_time = min(TIMESTAMP_CLASSIFIER_),
								  end_time = max(TIMESTAMP_CLASSIFIER_),
								  min_order = min(.order)) -> base_log
	}

	#create end points for graph



	base_log %>%
		group_by(CASE_CLASSIFIER_) %>%
		arrange(start_time, min_order) -> points_temp

	points_temp %>%
		slice(1) %>%
		mutate(ACTIVITY_CLASSIFIER_ = "Start",
			   end_time = start_time,
			   min_order = -Inf) -> end_points_start
	points_temp %>%
		slice(n()) %>%
		mutate(ACTIVITY_CLASSIFIER_ = "End",
			   start_time = end_time,
			   min_order = Inf) -> end_points_end

	bind_rows(end_points_start, end_points_end) -> end_points
	#
	# #add endpoints to base log
	#
	suppressWarnings(base_log  %>%
					 	bind_rows(end_points) -> base_log)

	#create base nodes list

	base_log %>%
		ungroup() %>%
		count(ACTIVITY_CLASSIFIER_) %>%
		mutate(node_id = 1:n()) -> base_nodes

	#create base precedence list

	suppressWarnings(base_log %>%
					 	ungroup() %>%
					 	mutate(ACTIVITY_CLASSIFIER_ = ordered(ACTIVITY_CLASSIFIER_, levels = c("Start", as.character(sort(activity_labels(eventlog))), "End"))) %>%
					 	group_by(CASE_CLASSIFIER_) %>%
					 	arrange(start_time, min_order) %>%
					 	mutate(next_act = lead(ACTIVITY_CLASSIFIER_),
					 		   next_start_time = lead(start_time),
					 		   next_end_time = lead(end_time)) %>%
					 	full_join(base_nodes, by = c("ACTIVITY_CLASSIFIER_" = "ACTIVITY_CLASSIFIER_")) %>%
					 	rename(from_id = node_id) %>%
					 	full_join(base_nodes, by = c("next_act" = "ACTIVITY_CLASSIFIER_")) %>%
					 	rename(to_id = node_id) %>%
					 	select(-n.x, -n.y) %>%
					 	ungroup() -> base_precedence)

	extra_data <- list()
	extra_data$n_cases <- n_cases(eventlog)
	extra_data$n_activity_instances <- n_activity_instances(eventlog)


	edges <- attr(type, "create_edges")(base_precedence, type, extra_data) %>%
		attr(type, "transform_for_matrix")(type, extra_data)

	class(edges) <- c("process_matrix", class(edges))
	attr(edges, "matrix_type") <- type



	return(edges)

}
