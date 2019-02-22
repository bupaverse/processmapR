

create_base_precedence <- function(eventlog, type_nodes, type_edges) {

	min_order <- NULL
	ACTIVITY_CLASSIFIER_ <- NULL
	ACTIVITY_INSTANCE_CLASSIFIER_ <- NULL
	CASE_CLASSIFIER_ <- NULL
	TIMESTAMP_CLASSIFIER_ <- NULL
	start_time <- NULL
	end_time <- NULL
	node_id <- NULL
	n.x <- NULL
	n.y <- NULL
	from_id <- NULL
	tooltip <- NULL
	label <- NULL
	next_act <- NULL
	to_id <- NULL
	duration <- NULL
	value <- NULL
	color_level <- NULL
	sec_label <- NULL
	node_id.y <- NULL
	node_id.x <- NULL


	eventlog <- ungroup_eventlog(eventlog)

	eventlog %>%
		as.data.frame() %>%
		droplevels %>%
		select(ACTIVITY_CLASSIFIER_ = !!activity_id_(eventlog),
			   ACTIVITY_INSTANCE_CLASSIFIER_ = !!activity_instance_id_(eventlog),
			   CASE_CLASSIFIER_ = !!case_id_(eventlog),
			   TIMESTAMP_CLASSIFIER_ = !!timestamp_(eventlog),
			   .order,
			   everything()) -> grouped_log

	perspective_nodes <- attr(type_nodes, "perspective")
	perspective_edges <- attr(type_edges, "perspective")

	#create base_log: list of case > activity > instance - start + end + min + order (+ custom attributes)

	if(perspective_nodes == "custom" && perspective_edges == "custom") {
		attributeNode <- attr(type_nodes, "attribute")
		attributeEdge <- attr(type_edges, "attribute")
		# grouped_log %>% summarize(start_time = min(TIMESTAMP_CLASSIFIER_),
		# 						  end_time = max(TIMESTAMP_CLASSIFIER_),
		# 						  min_order = min(.order),
		# 						  !!attributeNode := first(!!attributeNode),
		# 						  !!attributeEdge := first(!!attributeEdge)) -> base_log

		grouped_log <- data.table::data.table(grouped_log)

		base_log <- grouped_log[, list('start_time' = min(TIMESTAMP_CLASSIFIER_),
									   'end_time' = max(TIMESTAMP_CLASSIFIER_),
									   'min_order' = min(.order),
									   ATTRNode = first(get(attributeNode)),
									   ATTREdge = first(get(attributeEdge))),
								by = list(ACTIVITY_CLASSIFIER_, ACTIVITY_INSTANCE_CLASSIFIER_, CASE_CLASSIFIER_)]

		base_log <- tbl_df(base_log) %>%
			rename(!!sym(attributeNode) := "ATTRNode") %>%
			rename(!!sym(attributeEdge) := "ATTREdge")


	} else if(perspective_nodes == "custom") {
		attribute <- attr(type_nodes, "attribute")
		# grouped_log %>% summarize(start_time = min(TIMESTAMP_CLASSIFIER_),
		# 						  end_time = max(TIMESTAMP_CLASSIFIER_),
		# 						  min_order = min(.order),
		# 						  !!attribute := first(!!attribute)) -> base_log

		grouped_log <- data.table::data.table(grouped_log)

		base_log <- grouped_log[, list('start_time' = min(TIMESTAMP_CLASSIFIER_),
									   'end_time' = max(TIMESTAMP_CLASSIFIER_),
									   'min_order' = min(.order),
									   ATTR = first(get(attribute))),
								by = list(ACTIVITY_CLASSIFIER_,
										  ACTIVITY_INSTANCE_CLASSIFIER_,
										  CASE_CLASSIFIER_)]

		base_log <- tbl_df(base_log) %>%
			rename(!!sym(attribute) := "ATTR")

	} else if (perspective_edges == "custom") {
		attribute <- attr(type_edges, "attribute")
		# grouped_log %>% summarize(start_time = min(TIMESTAMP_CLASSIFIER_),
		# 						  end_time = max(TIMESTAMP_CLASSIFIER_),
		# 						  min_order = min(.order),
		# 						  !!attribute := first(!!attribute)) -> base_log


		grouped_log <- data.table::data.table(grouped_log)

		base_log <- grouped_log[, list('start_time' = min(TIMESTAMP_CLASSIFIER_),
									   'end_time' = max(TIMESTAMP_CLASSIFIER_),
									   'min_order' = min(.order),
									   ATTR = first(get(attribute))),
								by = list(ACTIVITY_CLASSIFIER_,
										  ACTIVITY_INSTANCE_CLASSIFIER_,
										  CASE_CLASSIFIER_)]
		base_log <- tbl_df(base_log) %>%
			rename(!!sym(attribute) := "ATTR")

	} else {
		grouped_log <- data.table::data.table(grouped_log)

		base_log <- grouped_log[, list('start_time' = min(TIMESTAMP_CLASSIFIER_), 'end_time' = max(TIMESTAMP_CLASSIFIER_), 'min_order' = min(.order)),
					by = list(ACTIVITY_CLASSIFIER_, ACTIVITY_INSTANCE_CLASSIFIER_, CASE_CLASSIFIER_)]

		base_log <- tbl_df(base_log)
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

	#add endpoints to base log

	suppressWarnings(
		bind_rows(end_points_start, end_points_end, base_log) -> base_log
	)

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
					 	full_join(base_nodes, by = c("next_act" = "ACTIVITY_CLASSIFIER_")) %>%
					 	ungroup() %>%
					 	select(everything(),
					 		   -n.x, -n.y,
					 		   from_id = node_id.x,
					 		   to_id = node_id.y) -> base_precedence)

}
