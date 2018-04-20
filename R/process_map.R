
#' @title Process Map
#'
#'
#' @description A function for creating a process map of an event log.
#' @param eventlog The event log object for which to create a process map
#' @param type A process map type, which can be created with the functions frequency and performance. The first type focusses on the frequency aspect of a process, while the second one focussed on processing time.
#' @param type_edges A process map type to be used for edges only, which can be created with the functions frequency and performance. The first type focusses on the frequency aspect of a process, while the second one focussed on processing time.
#' @param type_nodes A process map type to be used for nodes only, which can be created with the functions frequency and performance. The first type focusses on the frequency aspect of a process, while the second one focussed on processing time.
#' @param rankdir The direction in which to layout the graph: "TB", "LR", "BT", "RL", corresponding to directed graphs drawn from top to bottom, from left to right, from bottom to top, and from right to left, respectively.

#' @param render Whether the map should be rendered immediately (default), or rather an object of type dgr_graph should be returned.
#' @param ... Deprecated arguments
#'
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(patients)
#' process_map(patients)
#' }
#' @export process_map




process_map <- function(eventlog, type = frequency("absolute"), type_nodes = type, type_edges = type, rankdir = "TB", render = T, fixed_edge_width = F, ...) {

	min_order <- NULL
	act <- NULL
	aid <- NULL
	case <- NULL
	time <- NULL
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

	eventlog %>%
		as.data.frame() %>%
		droplevels %>%
		select(act = !!activity_id_(eventlog),
			   aid = !!activity_instance_id_(eventlog),
			   case = !!case_id_(eventlog),
			   time = !!timestamp_(eventlog),
			   .order,
			   everything()) %>%
		group_by(act, aid, case) -> grouped_log

	perspective_nodes <- attr(type_nodes, "perspective")
	perspective_edges <- attr(type_edges, "perspective")

	if(perspective_nodes == "custom" && perspective_edges == "custom") {
		attributeNode <- sym(attr(type_nodes, "attribute"))
		attributeEdge <- sym(attr(type_edges, "attribute"))
		grouped_log %>% summarize(start_time = min(time),
								  end_time = max(time),
								  min_order = min(.order),
								  !!attributeNode := first(!!attributeNode),
								  !!attributeEdge := first(!!attributeEdge)) -> base_log
	} else if(perspective_nodes == "custom") {
		attribute <- sym(attr(type_nodes, "attribute"))
		grouped_log %>% summarize(start_time = min(time),
								  end_time = max(time),
								  min_order = min(.order),
								  !!attribute := first(!!attribute)) -> base_log
	} else if (perspective_edges == "custom") {
		attribute <- sym(attr(type_edges, "attribute"))
		grouped_log %>% summarize(start_time = min(time),
								  end_time = max(time),
								  min_order = min(.order),
								  !!attribute := first(!!attribute)) -> base_log
	} else {
		grouped_log %>% summarize(start_time = min(time),
								  end_time = max(time),
								  min_order = min(.order)) -> base_log
	}

	base_log %>%
		group_by(case) %>%
		arrange(start_time, min_order) -> points_temp

	points_temp %>%
		slice(c(1)) %>%
		mutate(act = "Start",
			   end_time = start_time,
			   min_order = -Inf) -> end_points_start
	points_temp %>%
		slice(c(n())) %>%
		mutate(act = "End",
			   start_time = end_time,
			   min_order = Inf) -> end_points_end

	bind_rows(end_points_start, end_points_end) -> end_points


	suppressWarnings(base_log  %>%
		bind_rows(end_points) -> base_log)

	base_log %>%
		ungroup() %>%
		count(act) %>%
		mutate(node_id = 1:n()) -> base_nodes

	suppressWarnings(base_log %>%
					 	ungroup() %>%
					 	mutate(act = ordered(act, levels = c("Start", as.character(sort(activity_labels(eventlog))), "End"))) %>%
					 	group_by(case) %>%
					 	arrange(start_time, min_order) %>%
					 	mutate(next_act = lead(act),
					 		   next_start_time = lead(start_time),
					 		   next_end_time = lead(end_time)) %>%
					 	full_join(base_nodes, by = c("act" = "act")) %>%
					 	rename(from_id = node_id) %>%
					 	full_join(base_nodes, by = c("next_act" = "act")) %>%
					 	rename(to_id = node_id) %>%
					 	select(-n.x, -n.y) %>%
					 	ungroup() -> base_precedence)


	if_end <- function(node, true, false) {
		ifelse(node %in% c("Start","End"), true, false)
	}
	if_start <- function(node, true, false) {
		ifelse(node %in% c("Start"), true, false)
	}


	nodes_performance <- function(precedence, type) {

		precedence %>%
			mutate(duration = as.double(end_time-start_time, units = attr(type, "units"))) %>%
			group_by(act, from_id) %>%
			summarize(label = type(duration, na.rm = T)) %>%
			na.omit() %>%
			ungroup() %>%
			mutate(color_level = label,
				   shape = if_end(act,"circle","rectangle"),
				   fontcolor = if_end(act, if_start(act, "chartreuse4","brown4"),  ifelse(label <= (min(label) + (5/8)*diff(range(label))), "black","white")),
				   color = if_end(act, if_start(act, "chartreuse4","brown4"),"grey"),
				   tooltip = paste0(act, "\n (", round(label, 2), " ",attr(type, "units"),")"),
				   label = if_end(act, act, tooltip))
	}


	nodes_frequency <- function(precedence, type, n_cases, n_activity_instances) {

		precedence %>%
			group_by(act, from_id) %>%
			summarize(n = as.double(n()),
					  n_distinct_cases = as.double(n_distinct(case))) %>%
			ungroup() %>%
			mutate(label = case_when(type == "relative" ~ 100*n/n_activity_instances,
									 type == "absolute" ~ n,
									 type == "absolute_case" ~ n_distinct_cases,
									 type == "relative_case" ~ 100*n_distinct_cases/n_cases)) %>%
			mutate(color_level = label,
				   shape = if_end(act,"circle","rectangle"),
				   fontcolor = if_end(act, if_start(act, "chartreuse4","brown4"),  ifelse(label <= (min(label) + (5/8)*diff(range(label))), "black","white")),
				   color = if_end(act, if_start(act, "chartreuse4","brown4"),"grey"),
				   tooltip = paste0(act, "\n (", round(label, 2), ifelse(type %in% c("absolute", "absolute_case"),"", "%"),")"),
				   label = if_end(act, act, tooltip)) %>%
			na.omit()
	}

	nodes_custom <- function(precedence, type) {

		attribute <- sym(attr(type, "attribute"))

		precedence %>%
			group_by(act, from_id) %>%
			summarize(label = type(!!attribute, na.rm = T)) %>%
			na.omit() %>%
			ungroup() %>%
			mutate(color_level = label,
				   shape = if_end(act,"circle","rectangle"),
				   fontcolor = if_end(act, if_start(act, "chartreuse4","brown4"),  ifelse(label <= (min(label) + (5/8)*diff(range(label))), "black","white")),
				   color = if_end(act, if_start(act, "chartreuse4","brown4"),"grey"),
				   tooltip =  paste0(act, "\n (", round(label, 2), " ",attr(type, "units"),")"),
				   label = paste0(act, "\n (", round(label, 2), " ",attr(type, "units"),")"),
				   label = if_end(act, act, tooltip))
	}


	edges_performance <- function(precedence, type) {

		flow_time <- attr(type, "flow_time")

		precedence %>%
			ungroup() %>%
			mutate(time = case_when(flow_time == "inter_start_time" ~ as.double(next_start_time - start_time, units = attr(type, "units")),
									flow_time == "idle_time" ~ as.double(next_start_time - end_time, units = attr(type, "units")))) %>%
			group_by(act, next_act, from_id, to_id) %>%
			summarize(value = type(time, na.rm = T),
					  label = paste0(round(type(time, na.rm = T),2), " ", attr(type, "units"))) %>%
			na.omit() %>%
			ungroup() %>%
			mutate(penwidth = rescale(value, to = c(1,5))) %>%
			mutate(label = if_end(act, " ", if_end(next_act, " ", label))) %>%
			select(-value)
	}

	edges_frequency <- function(precedence, type, n_cases) {
		precedence %>%
			ungroup() %>%
			group_by(act, from_id, next_act, to_id) %>%
			summarize(n = as.double(n()),
					  n_distinct_cases = as.double(n_distinct(case))) %>%
			na.omit() %>%
			group_by(act, from_id) %>%
			mutate(label = case_when(type == "relative" ~ round(100*n/sum(n),2),
									 type == "absolute" ~ n,
									 type == "absolute_case" ~ n_distinct_cases,
									 type == "relative_case" ~ round(100*n_distinct_cases/n_cases, 2))) %>%
			ungroup() %>%
			mutate(penwidth = rescale(label, to = c(1,5), from = c(0, max(label))))
	}

	edges_custom <- function(precedence, type) {

		attribute <- sym(attr(type, "attribute"))

		precedence %>%
			ungroup() %>%
			group_by(act, next_act, from_id, to_id) %>%
			summarize(value = type(!!attribute, na.rm = T),
					  label = round(type(!!attribute, na.rm = T),2)) %>%
			na.omit() %>%
			ungroup() %>%
			mutate(penwidth = rescale(value, to = c(1,5))) %>%
			mutate(label = paste0(label, " ", attr(type, "units"))) %>%
			select(-value)

	}


	if(perspective_nodes == "frequency")
		nodes_frequency(base_precedence, type_nodes, n_cases(eventlog), n_activity_instances(eventlog)) -> nodes
	else if(perspective_nodes == "performance")
		nodes_performance(base_precedence, type_nodes) -> nodes
	else if(perspective_nodes == "custom")
		nodes_custom(base_precedence, type_nodes) -> nodes


	if(perspective_edges == "frequency")
		edges_frequency(base_precedence, type_edges, n_cases(eventlog)) -> edges
	else if(perspective_edges == "performance")
		edges_performance(base_precedence, type_edges) -> edges
	else if(perspective_edges == "custom")
		edges_custom(base_precedence, type_edges) -> edges


	if(fixed_edge_width) {
		edges %>% mutate(penwidth = 1) -> edges
	}



	nodes %>%
		mutate(color_level = rescale(color_level, from = c(0, max(color_level)))) %>%
		mutate(color_level = if_end(act, Inf, color_level)) -> nodes


#
#
# 	if(!force && nrow(edges) > 750) {
# 		message("You are about to draw a large process map.
# 				This might take a long time and render unreadable. Try to filter your event log. Are you sure you want to proceed?")
# 		answer <- readline("Y/N: ")
#
# 		if(answer != "Y")
# 			break()
# 	}

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
				   fontname = "Arial") -> nodes_df

	min_level <- min(nodes_df$color_level)
	max_level <- max(nodes_df$color_level[nodes_df$color_level < Inf])

	create_edge_df(from = edges$from_id,
				   to = edges$to_id,
				   label = edges$label,
				   penwidth = edges$penwidth,
				   color = ifelse(perspective_edges == "performance", "red4", "dodgerblue4"),
				   fontname = "Arial") -> edges_df

	create_graph(nodes_df, edges_df) %>%
		add_global_graph_attrs(attr = "rankdir", value = rankdir,attr_type = "graph") %>%
		add_global_graph_attrs(attr = "layout", value = "dot", attr_type = "graph") %>%
		colorize_node_attrs(node_attr_from = "color_level",
							node_attr_to = "fillcolor",
							palette = attr(type_nodes, "color"),
							default_color = "white",
							cut_points = seq(min_level-0.1, max_level+.1, length.out = 9)) -> graph


	if(render == T) {
		graph %>% render_graph() -> graph
		attr(graph, "base_precedence") <- base_precedence

		graph %>% return()
	} else  {
		attr(graph, "base_precedence") <- base_precedence
		graph %>% return()
	}

}
