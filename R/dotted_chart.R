#' @title Dotted Chart
#'
#' @description
#' A dotted chart is a graph in which each activity instance is displayed with a point (dot). The x-axis refers to the
#' time aspect, while the y-axis refers to cases.
#'
#' @param log [`log`][`bupaR::log`]: Object of class [`log`][`bupaR::log`] or derivatives ([`grouped_log`][`bupaR::grouped_log`],
#' [`eventlog`][`bupaR::eventlog`], [`activitylog`][`bupaR::activitylog`], etc.).
#' @param x [`character`] (default `"absolute"`): Value to plot on x-axis: `"absolute"` time or `"relative"` time
#' (since start of week: `"relative_week"`, since start of day: `"relative_day"`).
#' @param sort [`character`] (default `"auto"`): Ordering of the cases on y-axis: `"auto"` (default, see **Details**),
#' `"start"`, `"end"`, `"duration"`, `"start_week"`, or `"start_day"`.
#' @param color [`character`] (default [`NULL`]): Attribute to use for coloring the activity instances (dots).
#' This attribute should be present in `log`. Default ([`NULL`]) is the activity identifier ([`activity_id()`][`bupaR::activity_id()`]).
#' Use [`NA`] for no colors.
#' @param units [`character`] (default `"auto"`): Time units to use on the x-axis in case of relative time: `"auto"`
#' (default, see **Details**), `"secs"`, `"mins"`, `"hours"`, `"days"`, or `"weeks"`.
#' @param add_end_events [`logical`] (default `FALSE`): Whether to add dots for the complete lifecycle event with a different shape.
#' @param scale_color `ggplot2` scale function (default [`scale_color_discrete_bupaR`][`bupaR::scale_color_discrete_bupaR`]):
#' Set color scale. Defaults to [`scale_color_discrete_bupaR`][`bupaR::scale_color_discrete_bupaR`].  Replaced with [`scale_color_discrete`][`ggplot2::scale_color_discrete`] when more than 26 activities are present.
#' @param plotly [`logical`] (default `FALSE`): Return a `plotly` object, instead of a `ggplot2`.
#' @param eventlog `r lifecycle::badge("deprecated")`; please use `log` instead.
#'
#' @details
#' When setting `sort` to `"auto"`, the ordering of cases is done automatically, based on the specified value of `x`:
#' * `x = "absolute"`: `sort = "start"`,
#' * `x = "relative"`: `sort = "duration"`,
#' * `x = "relative_week"`: `sort = "start_week"`,
#' * `x = "relative_day"`: `sort = "start_day"`.
#'
#' When setting `units` to `"auto"`, the time units on the x-axis is done automatically, based on the specified value of `x`:
#' * `x = "absolute"`: `units = "weeks"`,
#' * `x = "relative"`: `units = "weeks"`,
#' * `x = "relative_week"`: `units = "secs"`,
#' * `x = "relative_day"`: `units = "secs"`.
#'
#' @examples
#' library(processmapR)
#' library(eventdataR)
#'
#' patients %>%
#'  dotted_chart(x = "absolute", sort = "start", color = "employee")
#'
#' @name dotted_chart
#' @export dotted_chart
dotted_chart <- function(log,
                         x = c("absolute","relative","relative_week","relative_day"),
                         sort = c("auto","start","end","duration","start_week","start_day"),
                         color = NULL,
                         units = c("auto","secs","mins","hours","days","weeks"),
                         add_end_events = FALSE,
                         scale_color = bupaR::scale_color_discrete_bupaR,
                         plotly = FALSE,
                         eventlog = deprecated()) {
  UseMethod("dotted_chart")
}

#' @describeIn dotted_chart Create dotted chart for an [`eventlog`][`bupaR::eventlog`].
#' @export
dotted_chart.eventlog <- function(log,
                                  x = c("absolute","relative","relative_week","relative_day"),
                                  sort = c("auto","start","end","duration","start_week","start_day"),
                                  color = NULL,
                                  units = c("auto","secs","mins","hours","days","weeks"),
                                  add_end_events = FALSE,
                                  scale_color = bupaR::scale_color_discrete_bupaR,
                                  plotly = FALSE,
                                  eventlog = deprecated()) {

  if(lifecycle::is_present(eventlog)) {
    lifecycle::deprecate_warn("0.4.0", "dotted_chart(eventlog)", "(dotted_chart(log)")

    log <- eventlog
  }

  x <- arg_match(x)
  sort <- arg_match(sort)
  units <- arg_match(units)
  mapping <- mapping(log)

  dotted_chart_check_args(log, x, sort, units, color)

  log %>%
    dotted_chart_data(color, units) %>%
    dotted_chart_plot(mapping, x, sort, scale_color, color, units, add_end_events) -> p

  return_plotly(p, plotly)
}

#' @describeIn dotted_chart Create dotted chart for an [`activitylog`][`bupaR::activitylog`].
#' @export
dotted_chart.activitylog <- function(log,
                                     x = c("absolute","relative","relative_week","relative_day"),
                                     sort = c("auto","start","end","duration","start_week","start_day"),
                                     color = NULL,
                                     units = c("auto","secs","mins","hours","days","weeks"),
                                     add_end_events = FALSE,
                                     scale_color = bupaR::scale_color_discrete_bupaR,
                                     plotly = FALSE,
                                     eventlog = deprecated()) {

  log <- lifecycle_warning_eventlog(log, eventlog)

  dotted_chart.eventlog(to_eventlog(log), x, sort, color, units, add_end_events, scale_color, plotly)
}

#' @describeIn dotted_chart Create dotted chart for a [`grouped_eventlog`][`bupaR::grouped_eventlog`].
#' @export
dotted_chart.grouped_eventlog <- function(log,
                                          x = c("absolute","relative","relative_week","relative_day"),
                                          sort = c("auto","start","end","duration","start_week","start_day"),
                                          color = NULL,
                                          units = c("auto","secs","mins","hours","days","weeks"),
                                          add_end_events = FALSE,
                                          scale_color = bupaR::scale_color_discrete_bupaR,
                                          plotly = FALSE,
                                          eventlog = deprecated()) {

  if(lifecycle::is_present(eventlog)) {
    lifecycle::deprecate_warn("0.4.0", "dotted_chart(eventlog)", "dotted_chart(log)")

    log <- eventlog
  }

  x <- arg_match(x)
  sort <- arg_match(sort)
  units <- arg_match(units)
  mapping <- mapping(log)

  dotted_chart_check_args(log, x, sort, units, color)

  log %>%
    bupaR:::apply_grouped_fun(dotted_chart_data, color, units, .keep_groups = TRUE) %>%
    dotted_chart_plot(mapping, x, sort, scale_color, color, units, add_end_events) -> p

  return_plotly(p, plotly)
}

#' @describeIn dotted_chart Create dotted chart for a [`grouped_activitylog`][`bupaR::grouped_activitylog`].
#' @export
dotted_chart.grouped_activitylog <- function(log,
                                             x = c("absolute","relative","relative_week","relative_day"),
                                             sort = c("auto","start","end","duration","start_week","start_day"),
                                             color = NULL,
                                             units = c("auto","secs","mins","hours","days","weeks"),
                                             add_end_events = FALSE,
                                             scale_color = bupaR::scale_color_discrete_bupaR,
                                             plotly = FALSE,
                                             eventlog = deprecated()) {

  log <- lifecycle_warning_eventlog(log, eventlog)

  log %>%
    to_eventlog() %>%
    dotted_chart.grouped_eventlog(x, sort, color, units, add_end_events, scale_color, plotly)
}


#Check args and override values. Not the way you're supposed to do it, but it's straightforward and convenient!
dotted_chart_check_args <- function(log, x, sort, units, color, call = caller_env()) {

  if(sort == "auto") {
    assign("sort",
           switch(x,
                  "absolute"      = "start",
                  "relative"      = "duration",
                  "relative_week" = "start_week",
                  "relative_day"  = "start_day"),
           envir = parent.frame(1L))
  }

  if(units == "auto") {
    assign("units",
           switch(x,
                  "absolute"      = "weeks",
                  "relative"      = "weeks",
                  "relative_week" = "secs",
                  "relative_day"  = "secs"),
           envir = parent.frame(1L))
  }

  if(is.null(color)) {
    assign("color", activity_id(log), envir = parent.frame(1L))
  } else if(!is.na(color) && !(color %in% colnames(log))) {
    cli_abort(c("Invalid {.arg color}.",
                "x" = "{.val {color}} is not present in {.arg log}."),
              call = call)
  }

  # Just return something.
  return(TRUE)
}
