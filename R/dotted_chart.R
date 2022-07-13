#' @title Dotted chart
#' @description Create a dotted chart to view all events in a glance
#' @param log \code{\link[bupaR]{log}}: Object of class \code{\link[bupaR]{log}} or derivatives (\code{\link[bupaR]{grouped_log}}, \code{\link[bupaR]{eventlog}}, \code{\link[bupaR]{activitylog}}, etc.).
#' @param x Value for plot on x-axis: absolute time or relative time (since start, since start of week, since start of day)
#' @param sort Ordering of the cases on y-axis: start, end or duration, start_week, start_day
#' @param color Optional, variable to use for coloring dots. Default is the activity identifier. Use NA for no colors.
#' @param units Time units to use on x-axis in case of relative time.
#' @param plotly Return plotly object
#' @param add_end_events Whether to add dots for the complete lifecycle event with a different shape.
#' @param ... Deprecated arguments
#' @param eventlog Deprecated. Use log instead.
#' @importFrom tidyr spread
#' @importFrom data.table dcast.data.table
#' @importFrom data.table setorder
#' @importFrom data.table setnames
#' @importFrom bupaR to_eventlog
#' @export dotted_chart
#'


dotted_chart <- function(log,
						 x = c("absolute","relative","relative_week","relative_day"),
						 sort = NULL,
						 color = NULL,
						 units = NULL,
						 add_end_events = F,
						 eventlog = deprecated(),
						 ...) {
	UseMethod("dotted_chart")
}

#' @describeIn dotted_chart Create dotted chart for eventlog
#' @export


dotted_chart.eventlog <- function(log,
								  x = c("absolute","relative","relative_week","relative_day"),
								  sort = NULL,
								  color = NULL,
								  units = NULL,
								  add_end_events = F,
								  eventlog = deprecated(),
								  ...) {
	log <- lifecycle_warning_eventlog(log, eventlog)

	x <- match.arg(x)
	mapping <- mapping(log)

	if(is.null(sort)) {
		y <-	switch(x,
					"absolute" = "start",
					"relative" =  "duration",
					"relative_week" = "start_week",
					"relative_day" = "start_day")
	} else {
		y <-	match.arg(sort, choices = c("start","end","duration", "start_week","start_day"))
	}

	if(is.null(units)) {
		units <-	switch(x,
						"absolute" = "weeks",
						"relative" =  "weeks",
						"relative_week" = "secs",
						"relative_day" = "secs")
	} else {
		units <-	match.arg(units, choices = c("weeks","days","hours","mins","secs"))
	}




	log %>%
		dotted_chart_data(color, units) %>%
		dotted_chart_plot(mapping, x, y, col_vector(), ifelse(is.null(color), activity_id(log), color),
						  units, add_end_events = add_end_events)
}


#' @describeIn dotted_chart Create dotted chart for activitylog
#' @export

dotted_chart.activitylog <- function(log,
									 x = c("absolute","relative","relative_week","relative_day"),
									 sort = NULL,
									 color = NULL,
									 units = NULL,
									 add_end_events = F,
									 eventlog = deprecated(),
									 ...) {
	log <- lifecycle_warning_eventlog(log, eventlog)

	dotted_chart.eventlog(to_eventlog(log), x, sort, color, units, add_end_events)

}

#' @describeIn dotted_chart Create dotted chart for grouped eventlog
#' @export

dotted_chart.grouped_eventlog <- function(log,
										  x = c("absolute","relative","relative_week","relative_day"),
										  sort = NULL,
										  color = NULL,
										  units = NULL,
										  add_end_events = F,
										  eventlog = deprecated(),
										  ...) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	x <- match.arg(x)
	mapping <- mapping(log)

	if(is.null(sort)) {
		y <-	switch(x,
					"absolute" = "start",
					"relative" =  "duration",
					"relative_week" = "start_week",
					"relative_day" = "start_day")
	} else {
		y <-	match.arg(sort, choices = c("start","end","duration", "start_week","start_day"))
	}

	if(is.null(units)) {
		units <-	switch(x,
						"absolute" = "weeks",
						"relative" =  "weeks",
						"relative_week" = "secs",
						"relative_day" = "secs")
	} else {
		units <-	match.arg(units, choices = c("weeks","days","hours","mins","secs"))
	}

	log %>%
		bupaR:::apply_grouped_fun(dotted_chart_data, color, units, .keep_groups = TRUE) %>%
		dotted_chart_plot(mapping, x, y, col_vector(), ifelse(is.null(color), activity_id(log), color),
						  units, add_end_events = add_end_events)


}

#' @describeIn dotted_chart Create dotted chart for grouped activitylog
#' @export

dotted_chart.grouped_activitylog <- function(log,
											 x = c("absolute","relative","relative_week","relative_day"),
											 sort = NULL,
											 color = NULL,
											 units = NULL,
											 add_end_events = F,
											 eventlog = deprecated(),
											 ...) {

	log <- lifecycle_warning_eventlog(log, eventlog)

	x <- match.arg(x)
	mapping <- mapping(log)

	if(is.null(sort)) {
		y <-	switch(x,
					"absolute" = "start",
					"relative" =  "duration",
					"relative_week" = "start_week",
					"relative_day" = "start_day")
	} else {
		y <-	match.arg(sort, choices = c("start","end","duration", "start_week","start_day"))
	}

	if(is.null(units)) {
		units <-	switch(x,
						"absolute" = "weeks",
						"relative" =  "weeks",
						"relative_week" = "secs",
						"relative_day" = "secs")
	} else {
		units <-	match.arg(units, choices = c("weeks","days","hours","mins","secs"))
	}




	log %>%
		to_eventlog() %>%
		bupaR:::apply_grouped_fun(dotted_chart_data, color, units, .keep_groups = TRUE) %>%
	 	dotted_chart_plot(mapping, x, y, col_vector(), ifelse(is.null(color), activity_id(log), color),
	 	 				  units, add_end_events = add_end_events)




}

