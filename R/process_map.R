
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


	base_precedence <- create_base_precedence(eventlog, type_nodes, type_edges)

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
