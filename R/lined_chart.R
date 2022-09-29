#' @title Lined Chart
#'
#' @description
#' A lined chart is a graph in which each activity instance is displayed with a line. The x-axis refers to the time aspect,
#' while the y-axis refers to cases.
#'
#' @param x [`character`] (default `"absolute"`): Value to plot on x-axis: `"absolute"` time or `"relative"` time.
#' @param sort [`character`] (default `"auto"`): Ordering of the cases on y-axis: `"auto"` (default, see **Details**),
#' `"start"`, `"end"`, or `"duration"`.
#' @param line_width [`numeric`] (default `2`): The width of lines.
#'
#' @inheritParams dotted_chart
#'
#' @details
#' When setting `sort` to `"auto"`, the ordering of cases is done automatically, based on the specified value of `x`:
#' * `x = "absolute"`: `sort = "start"`,
#' * `x = "relative"`: `sort = "duration"`.
#'
#' When setting `units` to `"auto"`, the time units on the x-axis is done automatically, based on the specified value of `x`:
#' * `x = "absolute"`: `units = "weeks"`,
#' * `x = "relative"`: `units = "weeks"`.
#'
#' @seealso [`dotted_chart()`]
#'
#' @examples
#' library(processmapR)
#' library(eventdataR)
#'
#' patients %>%
#'  lined_chart(x = "absolute", color = "employee")
#'
#' @export lined_chart
lined_chart <- function(log,
                        x = c("absolute","relative"),
                        sort = c("auto","start","end","duration"),
                        color = NULL,
                        units = c("auto","secs","mins","hours","days","weeks"),
                        line_width = 2,
                        plotly = FALSE,
                        scale_color = bupaR::scale_color_discrete_bupaR,
                        eventlog = deprecated()) {
  UseMethod("lined_chart")
}

#' @describeIn lined_chart Create lined chart for an [`eventlog`][`bupaR::eventlog`].
#' @export
lined_chart.eventlog <- function(log,
                                 x = c("absolute","relative"),
                                 sort = c("auto","start","end","duration"),
                                 color = NULL,
                                 units = c("auto","secs","mins","hours","days","weeks"),
                                 line_width = 2,
                                 plotly = FALSE,
                                 scale_color = bupaR::scale_color_discrete_bupaR,
                                 eventlog = deprecated()) {

  log <- lifecycle_warning_eventlog(log, eventlog)

  x <- arg_match(x)
  sort <- arg_match(sort)
  units <- arg_match(units)
  mapping <- mapping(log)

  lined_chart_check_args(log, x, sort, units, color)

  log %>%
    lined_chart_data(color, units) %>%
    lined_chart_plot(mapping, x, sort, scale_color, color, units, line_width) -> p

  return_plotly(p, plotly)
}

#' @describeIn lined_chart Create lined chart for an [`activitylog`][`bupaR::activitylog`].
#' @export
lined_chart.activitylog <- function(log,
                                    x = c("absolute","relative"),
                                    sort = c("auto","start","end","duration"),
                                    color = NULL,
                                    units = c("auto","secs","mins","hours","days","weeks"),
                                    line_width = 2,
                                    plotly = FALSE,
                                    scale_color = bupaR::scale_color_discrete_bupaR,
                                    eventlog = deprecated()) {

  log <- lifecycle_warning_eventlog(log, eventlog)

  lined_chart.eventlog(to_eventlog(log), x, sort, color, units, line_width, plotly, scale_color)
}

#' @describeIn  lined_chart Create lined chart for a [`grouped_eventlog`][`bupaR::grouped_eventlog`].
#' @export
lined_chart.grouped_eventlog <- function(log,
                                         x = c("absolute","relative"),
                                         sort = c("auto","start","end","duration"),
                                         color = NULL,
                                         units = c("auto","secs","mins","hours","days","weeks"),
                                         line_width = 2,
                                         plotly = FALSE,
                                         scale_color = bupaR::scale_color_discrete_bupaR,
                                         eventlog = deprecated()) {

  log <- lifecycle_warning_eventlog(log, eventlog)

  groups <- groups(log)
  mapping <- mapping(log)

  if(is.null(color)) {
    color <- activity_id(log)

    log %>%
      group_by(!!sym(color), !!case_id_(log), .add = TRUE) -> log
  } else {
    log %>%
      group_by(!!case_id_(log), !!sym(color)) %>%
      summarize() %>%
      summarize(n = n()) %>%
      filter(n > 1) -> filter_color

    if(nrow(filter_color) > 0) {
      cli_abort(c("Invalid {.arg color}.",
                  "x" = "{.val {color}} is not present in {.arg log}."))
    } else {
      log %>% group_by(!!sym(color),
                       !!case_id_(log), .add = TRUE) -> log
    }
  }

  log %>%
    summarize(min = min(!!timestamp_(mapping)), max = max(!!timestamp_(mapping))) %>%
    ggplot(aes(x = min, xend = max, y = fct_reorder(!!case_id_(mapping), desc(min)), yend = fct_reorder(!!case_id_(mapping), desc(min)),
               group = !!case_id_(mapping))) +
    scale_y_discrete(breaks = NULL) +
    labs(x = "Time",y = "Cases") +
    theme_light() -> p

  if (is.na(color)) {
    p + geom_segment(lwd = line_width, color = "black") -> p
  } else {
    p + geom_segment(aes(color = factor(!!sym(color))), lwd = line_width) +
      scale_color(name = color) -> p
  }

  p <- p +
    facet_grid(as.formula(paste(c(paste(groups, collapse = "+"), "~." ), collapse = "")), scales = "free_y", space = "free")

  return_plotly(p, plotly)
}

#' @describeIn lined_chart Create lined chart for a [`grouped_activitylog`][`bupaR::grouped_activitylog`].
#' @export
lined_chart.grouped_activitylog <- function(log,
                                            x = c("absolute","relative"),
                                            sort = c("auto","start","end","duration"),
                                            color = NULL,
                                            units = c("auto","secs","mins","hours","days","weeks"),
                                            line_width = 2,
                                            plotly = FALSE,
                                            scale_color = bupaR::scale_color_discrete_bupaR,
                                            eventlog = deprecated()) {

  log <- lifecycle_warning_eventlog(log, eventlog)

  lined_chart.grouped_eventlog(to_eventlog(log), x, sort, color, units, line_width, plotly, scale_color)
}


#Check args and override values. Not the way you're supposed to do it, but it's straightforward and convenient!
lined_chart_check_args <- function(log, x, sort, units, color, call = caller_env()) {

  if(sort == "auto") {
    assign("sort",
           switch(x,
                  "absolute" = "start",
                  "relative" = "duration"),
           envir = parent.frame(1L))
  }

  if(units == "auto") {
    assign("units",
           switch(x,
                  "absolute" = "weeks",
                  "relative" = "weeks"),
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