#' @title Performance map profile
#' @description Function to create a performance map profile to be used as the type of a process map. It results in a process map describing process time.
#' @param FUN A summary function to be called on the process time of a specific activity, e.g. mean, median, min, max
#' @param units The time unit in which processing time should be presented (mins, hours, days, weeks, months, quarters, semesters, years. A month is defined as 30 days. A quarter is 13 weeks. A semester is 26 weeks and a year is 365 days
#' @param flow_time The time to depict on the flows: the inter start time is the time between the start timestamp of consecutive activity instances,
#' the idle time is the time between the end and start time of consecutive activity instances.
#' @param color_scale Name of color scale to be used for nodes. Defaults to Reds. See `Rcolorbrewer::brewer.pal.info()` for all options.
#' @param color_edges The color used for edges. Defaults to red4.
#' @export performance




performance <- function(FUN = mean, units = c("mins","hours","days","weeks", "months", "quarters", "semesters","years"), flow_time = c("idle_time","inter_start_time"), color_scale = "Reds", color_edges = "red4") {
	flow_time <- match.arg(flow_time)
	units <- match.arg(units)
	attr(FUN, "flow_time") <- flow_time
	attr(FUN, "perspective") <- "performance"

	attr(FUN, "units_label") <- units

	if(units %in% c("mins","hours","days","weeks")) {
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




	return(FUN)
}
