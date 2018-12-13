#' @title Performance map profile
#' @description Function to create a performance map profile to be used as the type of a process map. It results in a process map describing process time.
#' @param FUN A summary function to be called on the process time of a specific activity, e.g. mean, median, min, max
#' @param units The time unit in which processing time should be presented (mins, hours, days, weeks, months, quarters, semesters, years. A month is defined as 30 days. A quarter is 13 weeks. A semester is 26 weeks and a year is 365 days
#' @param flow_time The time to depict on the flows: the inter start time is the time between the start timestamp of consecutive activity instances,
#' the idle time is the time between the end and start time of consecutive activity instances.
#' @param color_scale Name of color scale to be used for nodes. Defaults to Reds. See `Rcolorbrewer::brewer.pal.info()` for all options.
#' @param color_edges The color used for edges. Defaults to red4.
#' @param ... Additional arguments too FUN
#' @export performance




performance <- function(FUN = mean,
						units = c("mins","secs", "hours","days","weeks", "months", "quarters", "semesters","years"),
						flow_time = c("idle_time","inter_start_time"),
						color_scale = "Reds",
						color_edges = "red4",
						...) {

	flow_time <- match.arg(flow_time)
	units <- match.arg(units)
	attr(FUN, "flow_time") <- flow_time
	attr(FUN, "perspective") <- "performance"

	attr(FUN, "units_label") <- units
	attr(FUN, "arguments") <- list(...)

	if(units %in% c("mins","hours","days","weeks", "secs")) {
		attr(FUN, "units") <- units
		attr(FUN, "scale_time") <- 1
	} else if (units == "months") {
		attr(FUN, "units") <- "days"
		attr(FUN, "scale_time") <- 1/30
	} else if (units == "semesters") {
		attr(FUN, "units") <- "days"
		attr(FUN, "scale_time") <- 1/(26*7)
	}
	else if (units == "years") {
		attr(FUN, "units") <- "days"
		attr(FUN, "scale_time") <- 1/(365)
	} else if(units == "quarters") {
		attr(FUN, "units") <- "days"
		attr(FUN, "scale_time") <- 1/(13*7)
	}

	attr(FUN, "color") <- color_scale
	attr(FUN, "color_edges") <- color_edges


	attr(FUN, "create_nodes") <- function(precedence, type, extra_data) {

		precedence %>%
			mutate(duration = as.double(end_time-start_time, units = attr(type, "units"))*attr(type, "scale_time")) %>%
			group_by(ACTIVITY_CLASSIFIER_, from_id) %>%
			summarize(label = do.call(function(...) type(duration, na.rm = T,...),  attr(type, "arguments"))) %>%
			na.omit() %>%
			ungroup() %>%
			mutate(color_level = label,
				   shape = if_end(ACTIVITY_CLASSIFIER_,"circle","rectangle"),
				   fontcolor = if_end(ACTIVITY_CLASSIFIER_, if_start(ACTIVITY_CLASSIFIER_, "chartreuse4","brown4"),  ifelse(label <= (min(label) + (5/8)*diff(range(label))), "black","white")),
				   color = if_end(ACTIVITY_CLASSIFIER_, if_start(ACTIVITY_CLASSIFIER_, "chartreuse4","brown4"),"grey"),
				   tooltip = paste0(ACTIVITY_CLASSIFIER_, "\n", round(label, 2), " ",attr(type, "units_label")),
				   label = if_end(ACTIVITY_CLASSIFIER_, ACTIVITY_CLASSIFIER_, tooltip))
	}

	attr(FUN, "create_edges") <- function(precedence, type, extra_data) {

		flow_time <- attr(type, "flow_time")

		precedence %>%
			ungroup() %>%
			mutate(time = case_when(flow_time == "inter_start_time" ~ as.double(next_start_time - start_time, units = attr(type, "units"))*attr(type, "scale_time"),
									flow_time == "idle_time" ~ as.double(next_start_time - end_time, units = attr(type, "units"))*attr(type, "scale_time"))) %>%
			group_by(ACTIVITY_CLASSIFIER_, next_act, from_id, to_id) %>%
			summarize(value = do.call(function(...) type(time, na.rm = T,...),  attr(type, "arguments"))) %>%
			mutate( label = paste0(round(value,2), " ", attr(type, "units_label"))) %>%
			na.omit() %>%
			ungroup() %>%
			mutate(penwidth = rescale(value, to = c(1,5))) %>%
			mutate(label = if_end(ACTIVITY_CLASSIFIER_, " ", if_end(next_act, " ", label))) %>%
			select(-value)
	}

	return(FUN)
}
