
# Utility functions for week/day caluclations
timeSinceStartOfWeek <- function(time) {
	midnight <- trunc(time, "days")
	weekDay <- as.integer(format(time, "%u")) - 1 # Week starts with Monday = 0
	secSinceMidnight <- timeSinceStartOfDay(time)
	as.difftime(secSinceMidnight + weekDay * 24 * 60 * 60, units = "secs")
}

timeSinceStartOfDay <- function(time) {
	midnight <- trunc(time, "days")
	difftime(time, midnight, units = "secs")
}

# Time formatter for the week and day options
timeFormat <- function(time){
	format(time, "%H:%M")
}

# Compute data for dotted_chart
dotted_chart_data <- function(eventlog, color, units) {
	time <- NULL
	case <- NULL
	activity <- NULL
	start_week <- NULL
	end_week <- NULL
	start_day <- NULL
	end_day <- NULL
	dur <- NULL
	start_case_week <- NULL
	start_case_day <- NULL
	start_relative <- NULL
	end_relative <- NULL
	start_case_rank <- NULL
	start <- NULL
	end <- NULL
	start_case <- NULL
	end_case <- NULL

	if (is.null(color)) {
		eventlog %>%
			mutate(color = !!activity_id_(eventlog)) -> eventlog
	} else if (is.na(color)) {
		eventlog %>%
			mutate(color = "undefined") -> eventlog
	} else {
		eventlog %>%
			mutate(color = !!sym(color)) -> eventlog
	}

	eventlog %>%

		rename(
			"time" = timestamp_(eventlog),
			"case" = case_id_(eventlog),
			"activity" = activity_id_(eventlog),
			"activity_instance_id" = activity_instance_id_(eventlog)
		) %>%
		as.data.table %>%
		.[, .(start = min(time), end = max(time)), .(case, activity, activity_instance_id, color)] %>%
		.[, .SD, by = case] -> grouped_activity_log


	grouped_activity_log %>%
		setorder(start) %>%
		.[, rank := paste0("ACTIVITY_RANKED_AS_", 1:.N), by = case] %>%
		.[, .(case, rank, start)] %>% dcast.data.table(case ~ rank,value.var = "start") %>%
		arrange_if(str_detect(names(.), "ACTIVITY_RANKED_AS_")) -> output
	output[, .(case, start_case_rank = 1:.N)][, .(case, start_case_rank)] -> eventlog_rank_start_cases

	grouped_activity_log <- grouped_activity_log[, start_week := as.double(timeSinceStartOfWeek(start), units = units)][,
																														end_week := as.double(timeSinceStartOfWeek(start), units = units)][,
																																														   start_day := as.double(timeSinceStartOfDay(start), units = units)][,
																																														   																   end_day := as.double(timeSinceStartOfDay(end), units = units)][,
																																														   																   															   start_case := min(start), by =case][, end_case := max(end), by = case][,
																																														   																   															   																	   dur := as.double(end_case - start_case, units = units)][,
																																														   																   															   																	   														start_case_week := timeSinceStartOfWeek(start_case)][,
																																														   																   															   																	   																											 start_case_day := timeSinceStartOfDay(start_case)][,
																																														   																   															   																	   																											 												   start_relative := as.double(start - start_case, units = units)][,
																																														   																   															   																	   																											 												   																end_relative := as.double(end - start_case, units = units)]

	grouped_activity_log %>% full_join(eventlog_rank_start_cases, by = "case") %>% .[, !"rank"] -> result
	setnames(result,c("case", "activity", "time", "activity_instance_id"),c(case_id(eventlog),activity_id(eventlog),timestamp(eventlog),activity_instance_id(eventlog)),skip_absent = TRUE)

	return(as.data.frame(result))
}

configure_x_aes <- function(x) {
	case_when(x == "absolute" ~ c("start","end"),
			  x == "relative" ~ c("start_relative", "end_relative"),
			  x == "relative_week" ~ c("start_week", "end_week"),
			  x == "relative_day" ~ c("start_day", "end_day"))
}

configure_y_aes <- function(y) {
	case_when(y == "start" ~ "start_case_rank",
			  y == "end" ~ "end_case",
			  y == "duration" ~ "dur",
			  y == "start_week" ~ "start_case_week",
			  y == "start_day" ~ "start_case_day")
}

configure_x_labs <- function(x, units) {
	case_when(x == "relative" ~ as.character(glue("Time since start case (in {units})")),
			  x == "relative_week" ~ as.character(glue("Time since start of week (monday) (in {units})")),
			  x == "relative_day" ~ as.character(glue("Time since start of day (in {units})")),
			  x == "absolute" ~ "Time")
}

dotted_chart_plot <- function(data, mapping, x, y, scale_color, col_label, units, add_end_events) {

	color <- NULL
	groups <- groups(data)
	x_aes <- configure_x_aes(x)
	y_aes <- configure_y_aes(y)
	x_labs <- configure_x_labs(x, units)


	if(length(unique(data$color)) > 26) {
		scale_color <- ggplot2::scale_color_discrete
	}

	data %>%
		ggplot(aes_string(x = x_aes[[1L]], y = glue("reorder({case_id(mapping)}, desc({y_aes}))"))) +
		scale_y_discrete(breaks = NULL) +
		labs(x = x_labs,y = "Cases") +
		theme_light() -> p

	if (is.na(col_label)) {
		p + geom_point(aes(shape = "start"), color = "black") -> p
	} else {
		p + geom_point(aes(color = color, shape = "start")) +
			scale_color(name = col_label) -> p
	}

	if (add_end_events) {
		if (is.na(col_label)) {
			p + geom_point(aes(x = !!sym(x_aes[[2]]), shape = "complete"), color = "black") +
				scale_shape_manual(name = "Lifecycle", values = c(1,16)) -> p
		} else {
			p + geom_point(aes(x = !!sym(x_aes[[2]]), color = color, shape = "complete")) +
				scale_shape_manual(name = "Lifecycle", values = c(1,16)) -> p
		}
	} else {
		p + scale_shape_discrete(guide= "none") -> p
	}

	if(x == "relative_week" && units == "secs") {
		p + scale_x_time(breaks = as_hms(seq(0, 7 * 86400, by = 8 * 3600)), labels = timeFormat) +
			geom_vline(xintercept = seq(0, 7 * 86400, by = 86400), colour="black")-> p
	} else if(x == "relative_day" && units == "secs") {
		p + scale_x_time(breaks = as_hms(seq(0, 86400, by = 2 * 3600))) -> p
	}

	if(length(groups) > 0)
		p + facet_grid(as.formula(paste(c(paste(groups, collapse = "+"), "~." ), collapse = "")), scales = "free_y", space = "free")
	else
		p
}

# col_vector <- function() {
# 	qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# 	unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# }
