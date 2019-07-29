
#' @title Process Map
#'
#'
#' @description A function for creating a process map of an event log.
#' @param eventlog The event log object for which to create a process map
#' @param type A process map type, which can be created with the functions frequency, performance and custom. The first type focusses on the frequency aspect of a process, while the second one focussed on processing time. The third one allows custom attributes to be used.

#' @param sec A secondary process map type. Values are shown between brackets.

#' @param type_edges A process map type to be used for edges only, which can be created with the functions frequency and performance. The first type focusses on the frequency aspect of a process, while the second one focussed on processing time.
#' @param type_nodes A process map type to be used for nodes only, which can be created with the functions frequency and performance. The first type focusses on the frequency aspect of a process, while the second one focussed on processing time.

#' @param sec_nodes A secondary process map type for nodes only.
#' @param sec_edges A secondary process map type for edges only.

#' @param rankdir The direction in which to layout the graph:  "LR" (default),"TB", "BT", "RL", corresponding to directed graphs drawn from top to bottom, from left to right, from bottom to top, and from right to left, respectively.

#' @param render Whether the map should be rendered immediately (default), or rather an object of type dgr_graph should be returned.
#' @param fixed_edge_width If TRUE, don't vary the width of edges.
#' @param fixed_node_pos When specified as a data.frame with three columns 'act', 'x', and 'y' the position of nodes is fixed. Note that his can only be used with the 'neato' layout engine.
#' @param ... Deprecated arguments
#'
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(patients)
#' process_map(patients)
#' }
#'
#' @importFrom purrr map
#' @importFrom data.table :=
#'
#' @export process_map


process_map <- function(eventlog, type, sec, type_nodes, type_edges, sec_nodes, sec_edges, rankdir,render, fixed_edge_width, fixed_node_pos, ...) {
	UseMethod("process_map")
}

#' @describeIn process_map Process map for event log
#' @export


process_map.eventlog <- function(eventlog,
								 type = frequency("absolute"),
								 sec = NULL,
								 type_nodes = type,
								 type_edges = type,
								 sec_nodes = sec,
								 sec_edges = sec,
								 rankdir = "LR",
								 render = T,
								 fixed_edge_width = F,
								 fixed_node_pos = NULL,
								 ...) {

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

	if (any(is.na(eventlog %>% pull(!!timestamp_(eventlog))))) {
		warning("Some of the timestamps in the supplied event log are missing (NA values). This may result in a invalid process map!")
	}


	#base_precedence <- create_base_precedence(eventlog, type_nodes, type_edges)

	eventlog <- ungroup_eventlog(eventlog)

	eventlog %>%
		as.data.frame() %>%
		droplevels %>%
		select(ACTIVITY_CLASSIFIER_ = !!activity_id_(eventlog),
			   ACTIVITY_INSTANCE_CLASSIFIER_ = !!activity_instance_id_(eventlog),
			   CASE_CLASSIFIER_ = !!case_id_(eventlog),
			   TIMESTAMP_CLASSIFIER_ = !!timestamp_(eventlog),
			   .order,
			   everything()) -> prepared_log

	perspective_nodes <- attr(type_nodes, "perspective")
	perspective_edges <- attr(type_edges, "perspective")

	#create base_log: list of case > activity > instance - start + end + min + order (+ custom attributes)

	group_log <- function(log) {
		group_by(log, ACTIVITY_CLASSIFIER_, ACTIVITY_INSTANCE_CLASSIFIER_, CASE_CLASSIFIER_)
	}

	if(perspective_nodes == "custom" && perspective_edges == "custom") {
		attributeNode <- sym(attr(type_nodes, "attribute"))
		attributeEdge <- sym(attr(type_edges, "attribute"))
		prepared_log %>%
			group_log() %>%
			summarize(start_time = min(TIMESTAMP_CLASSIFIER_),
					  end_time = max(TIMESTAMP_CLASSIFIER_),
					  min_order = min(.order),
					  !!attributeNode := first(!!attributeNode),
					  !!attributeEdge := first(!!attributeEdge)) -> base_log
	} else if(perspective_nodes == "custom") {
		attribute <- sym(attr(type_nodes, "attribute"))
		prepared_log %>%
			group_log() %>%
			summarize(start_time = min(TIMESTAMP_CLASSIFIER_),
					  end_time = max(TIMESTAMP_CLASSIFIER_),
					  min_order = min(.order),
					  !!attribute := first(!!attribute)) -> base_log
	} else if (perspective_edges == "custom") {
		attribute <- sym(attr(type_edges, "attribute"))
		prepared_log %>%
			group_log() %>%
			summarize(start_time = min(TIMESTAMP_CLASSIFIER_),
					  end_time = max(TIMESTAMP_CLASSIFIER_),
					  min_order = min(.order),
					  !!attribute := first(!!attribute)) -> base_log
	} else {
		# speed up standard aggregation by using data.table fast grouping (GForce)
		data.table::setDT(prepared_log)
		prepared_log[, list(start_time = min(TIMESTAMP_CLASSIFIER_),
							end_time = max(TIMESTAMP_CLASSIFIER_),
							min_order = min(.order)),
					 by = c("ACTIVITY_CLASSIFIER_", "ACTIVITY_INSTANCE_CLASSIFIER_", "CASE_CLASSIFIER_")] %>%
			as.data.frame() -> base_log
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
		bind_rows(end_points_start, end_points_end, base_log) %>%
			ungroup() -> base_log
	)

	#create base nodes list

	base_log %>%
		count(ACTIVITY_CLASSIFIER_) %>%
		mutate(node_id = 1:n()) -> base_nodes
	data.table::setDT(base_nodes, key = c("ACTIVITY_CLASSIFIER_"))

	#create base precedence list

	data.table::setDT(base_log, key = c("start_time", "min_order"))
	base_log[, ACTIVITY_CLASSIFIER_ := ordered(ACTIVITY_CLASSIFIER_,
											   levels = c("Start", as.character(sort(activity_labels(eventlog))), "End"))
	      	][, `:=`(next_act = data.table::shift(ACTIVITY_CLASSIFIER_, 1, type = "lead"),
	      			 next_start_time = data.table::shift(start_time, 1, type = "lead"),
	      			 next_end_time = data.table::shift(end_time, 1, type = "lead")),
	      	  by = CASE_CLASSIFIER_] %>%
	 	merge(base_nodes, by.x = c("ACTIVITY_CLASSIFIER_"), by.y = c("ACTIVITY_CLASSIFIER_"), all = TRUE) %>%
	 	merge(base_nodes, by.x = c("next_act"), by.y = c("ACTIVITY_CLASSIFIER_"), all = TRUE) %>%
		as.data.frame() %>%
	 	select(everything(),
	 		   -n.x, -n.y,
	 		   from_id = node_id.x,
	 		   to_id = node_id.y) -> base_precedence




	extra_data <- list()
	extra_data$n_cases <- n_cases(eventlog)
	extra_data$n_activity_instances <- n_activity_instances(eventlog)

	# primary info
	nodes <- attr(type_nodes, "create_nodes")(base_precedence, type_nodes, extra_data)
	edges <- attr(type_edges, "create_edges")(base_precedence, type_edges, extra_data)


	# secondary info
	if(!is.null(sec_nodes)) {
		nodes_secondary <- attr(sec_nodes, "create_nodes")(base_precedence, sec_nodes, extra_data) %>%
			select(ACTIVITY_CLASSIFIER_, from_id, label) %>%
			rename(sec_label = label)


		nodes %>%
			full_join(nodes_secondary, by = c("ACTIVITY_CLASSIFIER_", "from_id")) %>%
			mutate(label = if_end(ACTIVITY_CLASSIFIER_,
								  ACTIVITY_CLASSIFIER_,
								  str_replace(paste0(label, "\n","(", map(sec_label, ~str_split(.x, "\n")[[1]][2]), ")"), "\n\\(\\)",""))) -> nodes
	}

	if(!is.null(sec_edges)) {
		edges_secondary <- attr(sec_edges, "create_edges")(base_precedence, sec_edges, extra_data) %>%
			select(from_id, to_id, label) %>%
			rename(sec_label = label)

		edges %>%
			full_join(edges_secondary, by = c("from_id","to_id")) %>%
			mutate(label = str_replace(paste0(label, "\n (", sec_label, ')'), "\n \\( \\)","")) -> edges
	}

	if(fixed_edge_width) {
		edges %>% mutate(penwidth = 1) -> edges
	}



	nodes %>%
		mutate(color_level = rescale(color_level, from = c(0, max(color_level)))) %>%
		mutate(color_level = if_end(ACTIVITY_CLASSIFIER_, Inf, color_level)) -> nodes

	create_node_df(n = nrow(nodes),
				   label = nodes$label,
				   shape = nodes$shape,
				   color_level = nodes$color_level,
				   style = "rounded,filled",
				   fontcolor = nodes$fontcolor,
				   color = nodes$color,
				   tooltip = nodes$tooltip,
				   penwidth = 1.5,
				   fixedsize = FALSE,
				   fontname = "Arial",
				   fontsize = 10) -> nodes_df

	if (is.data.frame(fixed_node_pos)) {
		nodes %>%
			left_join(fixed_node_pos, by = c("ACTIVITY_CLASSIFIER_" = "act")) -> nodes
		nodes_df %>% mutate(x = nodes$x, y = nodes$y) -> nodes_df
	}

	min_level <- min(nodes_df$color_level)
	max_level <- max(nodes_df$color_level[nodes_df$color_level < Inf])

	create_edge_df(from = edges$from_id,
				   to = edges$to_id,
				   label = edges$label,
				   penwidth = edges$penwidth,
				   color = attr(type_edges, "color_edges"),
				   fontname = "Arial",
				   fontsize = 10) -> edges_df

	create_graph(nodes_df, edges_df) %>%
		add_global_graph_attrs(attr = "rankdir", value = rankdir,attr_type = "graph") %>%
		add_global_graph_attrs(attr = "layout", value = if_else(is.data.frame(fixed_node_pos), "neato", "dot"), attr_type = "graph") %>%
		colorize_node_attrs(node_attr_from = "color_level",
							node_attr_to = "fillcolor",
							palette = attr(type_nodes, "color"),
							default_color = "white",
							cut_points = seq(min_level-0.1, max_level+.1, length.out = 9)) -> graph

	base_precedence %>%
		rename(case = CASE_CLASSIFIER_,
			   aid = ACTIVITY_INSTANCE_CLASSIFIER_,
			   act = ACTIVITY_CLASSIFIER_) -> base_precedence


	if(render == T) {
		graph %>% render_graph() -> graph
		attr(graph, "base_precedence") <- base_precedence

		graph %>% return()
	} else  {
		attr(graph, "base_precedence") <- base_precedence
		graph %>% return()
	}

}

#' @describeIn process_map Process map for event log
#' @export


process_map.grouped_eventlog <- function(eventlog,
								 type = frequency("absolute"),
								 sec = NULL,
								 type_nodes = type,
								 type_edges = type,
								 sec_nodes = sec,
								 sec_edges = sec,
								 rankdir = "LR",
								 render = T,
								 fixed_edge_width = F,
								 fixed_node_pos = NULL,
								 ...) {
	m <- mapping(eventlog)

	eventlog %>%
		do(group_name = paste(select(., group_vars(eventlog)) %>%
							  	mutate_all(as.character) %>%
							  	distinct() %>%
							  	unique(), collapse = ","),
		   group_map = process_map(re_map(., m),
									 type = type,
									 sec = sec,
									 type_nodes = type_nodes,
									 type_edges = type_edges,
									 sec_nodes = sec_nodes,
									 sec_edges = sec_edges,
									 rankdir = rankdir,
									 render = F,
									 fixed_edge_width = fixed_edge_width,
									 fixed_node_pos = fixed_node_pos,
									 ...)) -> grouped_map

	if (render) {
		group_tags <-
			purrr::pmap(list(grouped_map$group_name, grouped_map$group_map),
						function(g_name, g_map) {
							rendered_map <- render_graph(graph = g_map, title = g_name,
														 width = "100%", height = "100%")
							htmltools::tags$div(rendered_map, style = "border: 1px dashed gray; margin: 1px; width: 100%")
						})
		doc <- htmltools::tags$div(style = "display: flex; flex-wrap: wrap",
								   group_tags)
		print(htmltools::browsable(doc))
		grouped_map
	} else {
		grouped_map
	}

}
