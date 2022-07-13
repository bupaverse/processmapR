

lined_chart_data <- function(eventlog, color, units) {
	start_case_rank <- NULL
	start <- NULL
	end <- NULL
	start_case <- NULL
	end_case <- NULL

	if(is.null(color)) {
		eventlog %>%
			mutate(color = "undefined") -> eventlog
	} else if(is.na(color)) {
		eventlog %>%
			mutate(color = "undefined") -> eventlog
	} else {
		eventlog %>%
			mutate(color = !!sym(color)) -> eventlog
	}

	eventlog %>%
		as.data.frame() %>%
		group_by(!!case_id_(eventlog),!!activity_id_(eventlog),!!activity_instance_id_(eventlog), color, .add = TRUE) %>%
		summarize(start = min(!!timestamp_(eventlog)),
				  end = max(!!timestamp_(eventlog))) %>%
		group_by(!!case_id_(eventlog)) -> grouped_activity_log


	grouped_activity_log %>%
		arrange(start) %>%
		mutate(rank = paste0("ACTIVITY_RANKED_AS_", 1:n())) %>%
		ungroup() %>%
		select(!!case_id_(eventlog), rank, start) %>%
		spread(rank, start) %>%
		arrange_if(str_detect(names(.), "ACTIVITY_RANKED_AS_")) %>%
		mutate(start_case_rank = 1:n()) %>%
		select(!!case_id_(eventlog), start_case_rank) -> eventlog_rank_start_cases

	grouped_activity_log %>%
		mutate(start_case = min(start),
			   end_case = max(end),
			   dur = as.double(end_case - start_case, units = units)) %>%
		mutate(start_relative = as.double(start - start_case, units = units),
			   end_relative = as.double(end - start_case, units = units)) %>%
		full_join(eventlog_rank_start_cases, by = case_id(eventlog))
}

configure_x_aes_lined <- function(x) {
	case_when(x == "absolute" ~ c("start","end"),
			  x == "relative" ~ c("start_relative", "end_relative"))
}

configure_y_aes_lined <- function(y) {
	case_when(y == "start" ~ "start_case_rank",
			  y == "end" ~ "end_case",
			  y == "duration" ~ "dur")
}

configure_x_labs_lined <- function(x, units) {
	case_when(x == "relative" ~ as.character(glue("Time since start case (in {units})")),
			  x == "absolute" ~ "Time")
}


lined_chart_plot <- function(data, mapping, x, y, col_vector, col_label, units, line_width) {

	color <- NULL
	x_aes <- configure_x_aes_lined(x)
	y_aes <- configure_y_aes_lined(y)
	x_labs <- configure_x_labs_lined(x, units)

	data %>%
		ggplot(aes_string(x = x_aes[[1]],
						  xend = x_aes[[2]],
						  y = glue("reorder({case_id(mapping)}, desc({y_aes}))"),
						  yend = glue("reorder({case_id(mapping)}, desc({y_aes}))"))) +
		scale_y_discrete(breaks = NULL) +
		labs(x = x_labs,y = "Cases") +
		theme_light() -> p

	p + geom_segment(aes(color = color), lwd = line_width) +
		scale_color_manual(name = col_label, values = col_vector)


}

