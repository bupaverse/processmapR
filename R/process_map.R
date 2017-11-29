
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

	if(n_traces(eventlog) > 750) {
		message("You are about to draw a process map with a lot of traces.
				This might take a long time. Try to filter your event log. Are you sure you want to proceed?")
		answer <- readline("Y/N: ")

		if(answer != "Y")
			break()
	}


	log <- droplevels(eventlog)

	log <- as.data.frame(log)
  
	log %>%
		mutate(node_id = as.numeric(as.factor(!!as.symbol(activity_id(eventlog))))) -> log

	log %>%
		group_by(!!as.symbol(case_id(eventlog))) %>%
		arrange(!!as.symbol(timestamp(eventlog))) %>%
		slice(1:1) %>%
		mutate(timestamp_classifier = (!!as.symbol(timestamp(eventlog))) - 1,
			   event_classifier = "Start",
			   node_id = 0) -> start_points
	
	log %>%
		group_by(!!as.symbol(case_id(eventlog))) %>%
		arrange(desc(!!as.symbol(timestamp(eventlog)))) %>%
		slice(1:1) %>%
		mutate(timestamp_classifier = (!!as.symbol(timestamp(eventlog))) + 1,
			   event_classifier = "End",
			   node_id = n_activities(eventlog)+1) -> end_points

	
	if(attr(type, "perspective") == "frequency") {
		suppressWarnings(log %>%
			rename(event_classifier = !!as.symbol(activity_id(eventlog)),
			       timestamp_classifier = !!as.symbol(timestamp(eventlog))) %>%
			bind_rows(start_points) %>%
			bind_rows(end_points) %>%
			group_by(!!as.symbol(activity_instance_id(eventlog)), event_classifier, node_id, !!as.symbol(case_id(eventlog))) %>%
			summarize(start_time = min(timestamp_classifier), end_time = max(timestamp_classifier)) %>%
			group_by(!!as.symbol(case_id(eventlog))) %>%
			arrange(start_time) %>%
			mutate(next_event = lead(event_classifier),
			       next_node_id = lead(node_id)) %>% 
			na.omit() -> precedences)
     
		precedences %>%
			group_by(event_classifier, node_id, next_event, next_node_id) %>%
			summarize(n = n()) %>%
			group_by(event_classifier, node_id) %>%
			mutate(rel_n = n/(sum(n))) -> edges
     
	} else if(attr(type, "perspective") == "performance") {
  		suppressWarnings(log %>%
			rename(event_classifier = !!as.symbol(activity_id(eventlog)),
			       timestamp_classifier = !!as.symbol(timestamp(eventlog))) %>%
			bind_rows(start_points) %>%
			bind_rows(end_points) %>%
			group_by(!!as.symbol(activity_instance_id(eventlog)), event_classifier, node_id, !!as.symbol(case_id(eventlog))) %>%
			summarize(start_time = min(timestamp_classifier), end_time = max(timestamp_classifier)) %>%
			group_by(!!as.symbol(case_id(eventlog))) %>%
			arrange(start_time) %>%
			mutate(next_event = lead(event_classifier),
			       next_node_id = lead(node_id),
			       ts_next_event = lead(start_time),
			       idle_time = as.double( (ts_next_event - start_time), units = attr(type, "units")) ) %>%
			na.omit() -> precedences)

		precedences %>%
			group_by(event_classifier, node_id, next_event, next_node_id) %>%
			summarize(n = n(),
				  mean_time = mean(idle_time),
				  median_time = median(idle_time)) %>%
			group_by(event_classifier, node_id) %>%
			mutate(rel_n = n/(sum(n))) -> edges
	}

	if(attr(type, "perspective") == "frequency") {
		if(type == "absolute") {
			edges %>%
				ungroup() %>%
				mutate(penwidth = 1 + 5*(n - min(n))/(max(n) - min(n))) -> edges
		}
		else {
			edges %>%
				ungroup() %>%
				mutate(n = round(rel_n*100, 2)) %>%
				mutate(penwidth = 1 + 5*(n - min(n))/(max(n) - min(n))) -> edges
		}

	} else if(attr(type, "perspective") == "performance") {
		edges %>%
			ungroup() %>%
	    mutate(penwidth = 1 + 5*(mean_time - min(mean_time))/as.integer(max(mean_time) - min(mean_time)),
	           color = gray(1 - ( ( (n - min(n))/(max(n) - min(n)) ) + 0.1*( 1- ((n - min(n))/(max(n) - min(n))) ) ))) -> edges
	} 
	
	if(attr(type, "perspective") == "frequency") {
		eventlog %>%
			activities() %>%
			arrange_(activity_id(eventlog)) -> nodes
	} else if(attr(type, "perspective") == "performance") {
		eventlog %>%
			processing_time("activity", units = attr(type, "units")) %>%
			attr("raw") %>%
			group_by(!!as.symbol(activity_id(eventlog))) %>%
			summarize(absolute_frequency = type(processing_time)) %>%
			arrange(!!as.symbol(activity_id(eventlog))) -> nodes
	}



	colnames(nodes)[colnames(nodes) == activity_id(eventlog)] <- "event_classifier"

	# nodes_df <- create_nodes(nodes = c(nodes$event, "Start","End"),
	# 						 label = c(paste0(nodes$event, " (",nodes$absolute_frequency, ")"), "Start","End"),
	# 						 shape = c(rep("rectangle", nrow(nodes)), rep("circle",2)),
	# 						 style = "rounded,filled",
	# 						 fontcolor = "white",
	# 						 color = "white",
	# 						 fillcolor = c(rep("dodgerblue4", nrow(nodes)), "green","red"),
	# 						 fontname = "Arial",
	# 						 tooltip = c(paste0(nodes$event, "\n (",nodes$absolute_frequency, ")"), "Start","End") )

	if(attr(type, "perspective") == "performance") {
		nodes_df <- create_node_df(n = nrow(nodes) + 2,
								   nodes = 0:(n_activities(eventlog) + 1),
								   label = c("Start", c(paste0(nodes$event_classifier, " (",round(nodes$absolute_frequency, 3), ")")),"End"),
								   shape = c("circle",rep("rectangle", nrow(nodes)), "circle"),
								   style = "rounded,filled",
								   fontcolor = c("green",rep("white", nrow(nodes)),"red"),
								   color = c("green",rep("grey", nrow(nodes)), "red"),
								   penwidth = 1.5,
								   frequency = c( -Inf, nodes$absolute_frequency, max(nodes$absolute_frequency)+2),
								   fillcolor = c("green",rep("dodgerblue4", nrow(nodes)),"red"),
								   fontname = "Arial",
								   tooltip = c(paste0(nodes$event_classifier, "\n (",nodes$absolute_frequency, ")"), "Start","End"))
	} else if(type == "absolute") {

		nodes_df <- create_node_df(n = nrow(nodes) + 2,
								   nodes = 0:(n_activities(eventlog) + 1),
								   label = c("Start", c(paste0(nodes$event_classifier, " (",nodes$absolute_frequency, ")")),"End"),
								   shape = c("circle",rep("rectangle", nrow(nodes)), "circle"),
								   style = "rounded,filled",
								   fontcolor = c("green",rep("white", nrow(nodes)),"red"),
								   color = c("green",rep("grey", nrow(nodes)), "red"),
								   penwidth = 1.5,
								   frequency = c( -Inf, nodes$absolute_frequency, max(nodes$absolute_frequency)+2),
								   fillcolor = c("green",rep("dodgerblue4", nrow(nodes)),"red"),
								   fontname = "Arial",
								   tooltip = c(paste0(nodes$event_classifier, "\n (",nodes$absolute_frequency, ")"), "Start","End"))

	} else {
		nodes_df <- create_node_df(n = nrow(nodes) + 2,
								   nodes = 0:(n_activities(eventlog) + 1),
								   label = c("Start", c(paste0(nodes$event_classifier, " (",round(100*nodes$relative_frequency,2), ")")),"End"),
								   shape = c("circle",rep("rectangle", nrow(nodes)), "circle"),
								   style = "rounded,filled",
								   fontcolor = c("green",rep("white", nrow(nodes)),"red"),
								   color = c("green",rep("grey", nrow(nodes)), "red"),
								   penwidth = 1.5,
								   frequency = c( -Inf, nodes$absolute_frequency, max(nodes$absolute_frequency)+2),
								   fillcolor = c("green",rep("dodgerblue4", nrow(nodes)),"red"),
								   fontname = "Arial",
								   tooltip = c("Start",paste0(nodes$event_classifier, "\n (",nodes$absolute_frequency, ")"), "End"))
	}
	
	if(attr(type, "perspective") == "frequency") {
  	edges_df <- create_edge_df(from = edges$node_id +1,
  							   to= edges$next_node_id + 1,
  							   label = edges$n,
  							   color = "grey",
  							   fontname = "Arial",
  							   arrowsize = 1,
  							   penwidth = edges$penwidth)
  	# edges_df <- create_edges(from = edges$event, to=edges$next_event,
  	# 						 label = edges$n,
  	# 						 color = "grey",
  	# 						 fontname = "Arial")
	} else if(attr(type, "perspective") == "performance") {
	  edges_df <- create_edge_df(from = edges$node_id +1,
	                             to= edges$next_node_id + 1,
	                             label = paste0("  ", round(edges$mean_time,2), " ", attr(type, "units"), "\n", 
	                                            "  ", round(edges$median_time,2), " ", attr(type, "units")),
	                             color = edges$color,
	                             fontname = "Arial",
	                             arrowsize = 1,
	                             penwidth = edges$penwidth)
	  # edges_df <- create_edges(from = edges$event, to=edges$next_event,
	  # 						 label = edges$n,
	  # 						 color = "grey",
	  # 						 fontname = "Arial")
	  }

	create_graph(nodes_df, edges_df) %>%
		set_global_graph_attrs(attr = "rankdir",value =  "LR",attr_type =  "graph") -> graph




	if(attr(type, "perspective") == "performance")
	{
		graph %>%
			colorize_node_attrs(node_attr_from = "frequency",
								node_attr_to = "fillcolor",
								palette = "Oranges",
								default_color = "white",
								reverse_palette = F,
								cut_points = seq(min(nodes$absolute_frequency) - 1 - 0.0001*(0.01+diff(range(nodes$absolute_frequency))),
												 max(nodes$absolute_frequency)  + 0.0001*(0.01+diff(range(nodes$absolute_frequency))),
												 length.out = 9)) -> graph
	} else {
		graph	%>%
			colorize_node_attrs(node_attr_from = "frequency",
								node_attr_to = "fillcolor",
								palette = "PuBu",
								default_color = "white",
								reverse_palette = F,
								cut_points = seq(min(nodes$absolute_frequency) - 0.63*(0.01+diff(range(nodes$absolute_frequency))),
												 max(nodes$absolute_frequency) + 1,
												 length.out = 9)) -> graph

	}

	if(render == T) {
		graph %>% render_graph() %>% return()
	} else
		graph %>% return()

}
