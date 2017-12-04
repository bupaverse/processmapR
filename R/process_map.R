
#' @title Process Map
#'
#'
#' @description A function for creating a process map of an event log.
#' @param eventlog The event log object for which to create a process map
#' @param type A process map type, which can be created with the functions frequency and performance. The first type focusses on the frequency aspect of a process, while the second one focussed on processing time.
#' @param render Whether the map should be rendered immediately (default), or rather an object of type dgr_graph should be returned.
#'
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(patients)
#' process_map(patients)
#' }
#' @export process_map




process_map <- function(eventlog, type = frequency("absolute") , render = T) {

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

	if(n_traces(eventlog) > 750) {
		message("You are about to draw a process map with a lot of traces.
				This might take a long time. Try to filter your event log. Are you sure you want to proceed?")
		answer <- readline("Y/N: ")

		if(answer != "Y")
			break()
	}


	eventlog %>%
		as.data.frame() %>%
		droplevels %>%
		select(act = !!activity_id_(eventlog),
			   aid = !!activity_instance_id_(eventlog),
			   case = !!case_id_(eventlog),
			   time = !!timestamp_(eventlog)) %>%
		group_by(act, aid, case) %>%
		summarize(start_time = min(time),
				  end_time = max(time)) -> base_log


	base_log %>%
		group_by(case) %>%
		arrange(start_time) %>%
		slice(c(1,n())) %>%
		mutate(act = c("Start","End")) %>%
		mutate(start_time = recode(act, "End" = end_time, .default = start_time)) %>%
		mutate(end_time = recode(act, "Start" = start_time, .default = end_time)) -> end_points


	base_log  %>%
		bind_rows(end_points) -> base_log

	base_log %>%
		ungroup() %>%
		count(act) %>%
		mutate(node_id = 1:n()) -> base_nodes

	suppressWarnings(base_log %>%
		ungroup() %>%
		mutate(act = ordered(act, levels = c("Start", as.character(activity_labels(eventlog)), "End"))) %>%
		group_by(case) %>%
		arrange(start_time, act) %>%
		mutate(next_act = lead(act),
			   next_start_time = lead(start_time),
			   next_end_time = lead(end_time)) %>%
		full_join(base_nodes, by = c("act" = "act")) %>%
		rename(from_id = node_id) %>%
		full_join(base_nodes, by = c("next_act" = "act")) %>%
		rename(to_id = node_id) %>%
		select(-n.x, -n.y) -> base_precedence)


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
			summarize(label = type(duration)) %>%
			na.omit() %>%
			ungroup() %>%
			mutate(color_level = label,
				   shape = if_end(act,"circle","rectangle"),
				   fontcolor = if_end(act, if_start(act, "chartreuse4","brown4"),  ifelse(label <= quantile(label, 0.65), "black","white")),
				   color = if_end(act, if_start(act, "chartreuse4","brown4"),"grey"),
				   tooltip = paste0(act, "\n (", round(label, 2), " ",attr(type, "units"),")"),
				   label = if_end(act, act, tooltip))
	}

	nodes_frequency <- function(precedence, type) {

		precedence %>%
			group_by(act, from_id) %>%
			summarize(n = as.double(n())) %>%
			ungroup() %>%
			mutate(label = case_when(type == "relative" ~ n/sum(n),
									 type == "absolute" ~ n)) %>%
			mutate(color_level = label,
				   shape = if_end(act,"circle","rectangle"),
				   fontcolor = if_end(act, if_start(act, "chartreuse4","brown4"),  ifelse(label <= quantile(label, 0.4), "black","white")),
				   color = if_end(act, if_start(act, "chartreuse4","brown4"),"grey"),
				   tooltip = paste0(act, "\n (", round(label, 2), ")"),
				   label = if_end(act, act, tooltip)) %>%
			na.omit()
	}


	edges_performance <- function(precedence, type) {

		flow_time <- attr(type, "flow_time")

		precedence %>%
			ungroup() %>%
			mutate(time = case_when(flow_time == "inter_start_time" ~ as.double(next_start_time - start_time, units = attr(type, "units")),
									flow_time == "idle_time" ~ as.double(next_start_time - end_time, units = attr(type, "units")))) %>%
			group_by(act, next_act, from_id, to_id) %>%
			summarize(value = type(time),
					  label = paste0(round(type(time),2), " ", attr(type, "units"))) %>%
			na.omit() %>%
			ungroup() %>%
			mutate(penwidth = rescale(value, to = c(1,5))) %>%
			mutate(label = if_end(act, "", if_end(next_act, "", label)))
	}

	edges_frequency <- function(precedence, type) {
		precedence %>%
			ungroup() %>%
			group_by(act, from_id, next_act, to_id) %>%
			summarize(n = as.double(n())) %>%
			na.omit() %>%
			group_by(act, from_id) %>%
			mutate(label = case_when(type == "relative" ~ round(100*n/sum(n),2),
									 type == "absolute" ~ n)) %>%
			ungroup() %>%
			mutate(penwidth = rescale(label, to = c(1,5)))
	}

	perspective <- attr(type, "perspective")


	if(perspective == "frequency") {
		nodes_frequency(base_precedence, type) -> nodes
	} else if(perspective == "performance")
		nodes_performance(base_precedence, type) -> nodes


	if(perspective == "frequency") {
		edges_frequency(base_precedence, type) -> edges
	} else if(perspective == "performance")
		edges_performance(base_precedence, type) -> edges


	nodes %>%
		mutate(color_level = rescale(color_level)) %>%
		mutate(color_level = if_end(act, Inf, color_level)) -> nodes


	create_node_df(n = nrow(nodes),
				   label = nodes$label,
				   shape = nodes$shape,
				   color_level = nodes$color_level,
				   style = "rounded,filled",
				   fontcolor = nodes$fontcolor,
				   color = nodes$color,
				   tooltip = nodes$tooltip,
				   penwidth = 1.5,
				   fontname = "Arial") -> nodes_df

	min_level <- min(nodes_df$color_level)
	max_level <- max(nodes_df$color_level[nodes_df$color_level < Inf])

	create_edge_df(from = edges$from_id,
				   to = edges$to_id,
				   label = edges$label,
				   penwidth = edges$penwidth,
				   color = ifelse(perspective == "performance", "red4", "dodgerblue4"),
				   fontname = "Arial") -> edges_df

	create_graph(nodes_df, edges_df) %>%
		set_global_graph_attrs(attr = "rankdir", value = "LR",attr_type = "graph") %>%
		colorize_node_attrs(node_attr_from = "color_level",
							node_attr_to = "fillcolor",
							palette = ifelse(perspective == "performance", "Reds", "PuBu"),
							default_color = "white",
							cut_points = seq(min_level-0.1, max_level+.1, length.out = 9)) -> graph


	if(render == T) {
		graph %>% render_graph() %>% return()
	} else
		graph %>% return()

}
