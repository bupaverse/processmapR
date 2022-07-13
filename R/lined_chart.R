#' @title Lined chart
#' @description Create a lined chart to view all cases at a glance
#' @param eventlog Eventlog object
#' @param color Optional. Should be a case attribute! No coloring applied by default.
#' @param plotly Return plotly object
#' @inheritParams dotted_chart
#' @param line_width The width of lines
#' @param ... Deprecated arguments
#' @export lined_chart
#'

lined_chart <- function(log,
						x = c("absolute","relative"),
						sort = NULL,
						color,
						units,
						line_width = 2,
						plotly,
						eventlog = deprecated(),
						...) {
	UseMethod("lined_chart")
}

#' @describeIn lined_chart Create lined chart for eventlog
#' @export
lined_chart.eventlog <- function(log,
								 x = c("absolute","relative"),
								 sort = NULL,
								 color = NULL,
								 units = c("weeks","days","hours","mins","secs"),
								 line_width = 2,
								 plotly = FALSE,
								 eventlog = deprecated(),
								 ...) {

	log <- lifecycle_warning_eventlog(log, eventlog)

	x <- match.arg(x)
	units <- match.arg(units)
	mapping <- mapping(log)

	if(is.null(sort)) {
		y <-	switch(x,
					"absolute" = "start",
					"relative" =  "duration")
	} else {
		y <-	match.arg(sort, choices = c("start","duration", "end"))
	}


	log %>%
		lined_chart_data(color, units) %>%
		lined_chart_plot(mapping, x, y, col_vector(), color, units, line_width = line_width)


}

#' @describeIn lined_chart Create lined chart for activity log
#' @export

lined_chart.activitylog <- function(log,
									x = c("absolute","relative"),
									sort = NULL,
									color = NULL,
									units = c("weeks","days","hours","mins","secs"),
									line_width = 2,
									plotly = FALSE,
									eventlog = deprecated(),
									...) {

	log <- lifecycle_warning_eventlog(log, eventlog)

	log %>%
		to_eventlog %>%
		lined_chart(x,
					sort,
					color,
					units,
					line_width,
					plotly,
					...)
}

#' @describeIn  lined_chart Create lined chart for grouped eventlog
#' @export

lined_chart.grouped_eventlog <- function(log,
										 x = c("absolute","relative"),
										 sort = NULL,
										 color = NULL,
										 units = c("weeks","days","hours","mins","secs"),
										 line_width = 2,
										 plotly = FALSE,
										 eventlog = deprecated(),
										 ...) {

	log <- lifecycle_warning_eventlog(log, eventlog)


	groups <- groups(log)
	mapping <- mapping(log)

	if(is.null(color)) {
		log %>%
			group_by(!!case_id_(log), .add = TRUE) -> log
	} else {
		log %>%
			group_by( !!case_id_(log), !!sym(color)) %>%
			summarize() %>%
			summarize(n = n()) %>%
			filter(n > 1) -> filter_color

		if(nrow(filter_color) > 0) {
			stop("Attribute given to color argument is not a case attribute")
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


	if(!is.null(color)) {
		p <- p +
			geom_segment(aes(color = factor(!!sym(color))))
	} else {
		p <- p +
			geom_segment()
	}

	p <- p +
		facet_grid(as.formula(paste(c(paste(groups, collapse = "+"), "~." ), collapse = "")), scales = "free_y", space = "free")

	return(p)
}

#' @describeIn lined_chart Create lined chart for grouped activitylog
#' @export

lined_chart.grouped_activitylog <- function(log,
											x = c("absolute","relative"),
											sort = NULL,
											color = NULL,
											units = c("weeks","days","hours","mins","secs"),
											line_width = 2,
											plotly = FALSE,
											eventlog = deprecated(),
											...) {

	log <- lifecycle_warning_eventlog(log, eventlog)

	log %>%
		to_eventlog %>%
		lined_chart(x,
					sort,
					color,
					units,
					line_width,
					plotly,
					...)
}
