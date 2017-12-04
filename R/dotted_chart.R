#' @title Dotted chart
#' @description Create a dotted chart to view all events in a glance
#' @param eventlog Eventlog object
#' @param x Value for plot on x-axis: absolute time or relative time (since start, since start of week, since start of day)
#' @param sort Ordering of the cases on y-axis: start, end or duration
#' @param color Optional, variable to use for coloring dots. Default is the activity identifier. Use NA for no colors.
#' @param units Time units to use on x-axis in case of relative time.
#' @param plotly Return plotly object
#' @param ... Deprecated arguments
#'
#' @export dotted_chart
#'


dotted_chart <- function(eventlog, x, sort, color, units, ...) {
	UseMethod("dotted_chart")
}

#Utility functions for week/day caluclations
timeSinceStartOfWeek <- function(time) {
	midnight <- trunc(time, "days")
	weekDay <- as.integer(format(time, "%w"))
	weekDay <- ifelse(weekDay, weekDay-1, 6) # Let week start with Monday
	msSinceMidnight <- difftime(time, midnight, units="secs")
	as.difftime(msSinceMidnight + weekDay*24*60*60, units = "secs")
}
timeSinceStartOfDay <- function(time) {
	midnight <- trunc(time, "days")
	difftime(time, midnight, units="secs")
}
# time formatter for the week and day options

timeFormat <- function(time){
	substr(format(as.hms(as.double(time, units = "secs") %% (24 * 60 * 60))),0,5)
}

# compute data for dotted_chart
dotted_chart_data <- function(eventlog, color, units) {

	start <- NULL
	end <- NULL
	start_case <- NULL
	end_case <- NULL
	color <- NULL

	if(is.null(color)) {
		eventlog %>%
			mutate(color = !!activity_id_(eventlog)) -> eventlog
	} else if(is.na(color)) {
		eventlog %>%
			mutate(color = "undefined") -> eventlog
	} else {
		eventlog %>%
			mutate(color = !!sym(color)) -> eventlog
	}

	eventlog %>%
		group_by(!!case_id_(eventlog),!!activity_id_(eventlog),!!activity_instance_id_(eventlog), color, add = T) %>%
		summarize(start = min(!!timestamp_(eventlog)),
				  end = max(!!timestamp_(eventlog))) %>%
		group_by(!!case_id_(eventlog)) %>%
		mutate(start_week = as.double(timeSinceStartOfWeek(start), units = units)) %>%
		mutate(start_day = as.double(timeSinceStartOfDay(start), units = units)) %>%
		mutate(start_case = min(start),
			   end_case = max(end),
			   dur = as.double(end_case - start_case, units = units)) %>%
		mutate(start_case_week = timeSinceStartOfWeek(start_case),
			   start_case_day = timeSinceStartOfDay(start_case)) %>%
		mutate(start_relative = as.double(start - start_case, units = units))
}

configure_x_aes <- function(x) {
	case_when(x == "absolute" ~ "start",
			  x == "relative" ~ "start_relative",
			  x == "relative_week" ~ "start_week",
			  x == "relative_day" ~ "start_day")
}

configure_y_aes <- function(y) {
	case_when(y == "start" ~ "start_case",
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

dotted_chart_plot <- function(data, mapping, x, y, col_vector, col_label, units) {

	color <- NULL
	x_aes <- configure_x_aes(x)
	y_aes <- configure_y_aes(y)
	x_labs <- configure_x_labs(x, units)

	data %>%
		ggplot(aes_string(x = x_aes, y = glue("reorder({case_id(mapping)}, desc({y_aes}))"))) +
		scale_y_discrete(breaks = NULL) +
		labs(x = x_labs,y = "Cases") +
		theme_light() -> p

	p + geom_point(aes(color = color)) + scale_color_manual(name = col_label, values = col_vector) -> p




	if(x == "relative_week" && units == "secs") {
		p + scale_x_time(breaks = as.hms(seq(0, 7 * 86400, by = 8 * 3600)), labels = timeFormat) +
			geom_vline(xintercept = seq(0, 7 * 86400, by = 86400), colour="black")-> p
	} else if(x == "relative_day" && units == "secs") {
		p + scale_x_time(breaks = as.hms(seq(0, 86400, by = 2 * 3600))) -> p
	}
	p
}

col_vector <- function() {
	qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
	unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
}

#' @describeIn dotted_chart Dotted chart for event log
#' @export


dotted_chart.eventlog <- function(eventlog,
								  x = c("absolute","relative","relative_week","relative_day"),
								  sort = c("start","end","duration", "start_week","start_day"),
								  color = NULL,
								  units = c("weeks","days","hours","mins","secs"),
								  ...) {

	x <- match.arg(x)
	units <- match.arg(units)
	sort <- match.arg(sort)
	sort <- deprecated_y_arg(sort, ...)
	mapping <- mapping(eventlog)
	y <- sort


	eventlog %>%
		dotted_chart_data(color, units) %>%
		dotted_chart_plot(mapping, x, y, col_vector(), ifelse(is.null(color), activity_id(eventlog), color), units)
}

#' @describeIn dotted_chart Dotted chart for grouped event log
#' @export

dotted_chart.grouped_eventlog <- function(eventlog,
										  x = c("absolute","relative","relative_week","relative_day"),
										  sort = c("start","end","duration", "start_week","start_day"),
										  color = NULL,
										  units = c("weeks","days","hours","mins","secs"),
										  ...) {



	groups <- groups(eventlog)

	x <- match.arg(x)
	units <- match.arg(units)
	sort <- match.arg(sort)
	sort <- deprecated_y_arg(sort, ...)
	mapping <- mapping(eventlog)
	y <- sort

	eventlog %>%
		dotted_chart_data(color, units) %>%
		dotted_chart_plot(mapping, x, y, col_vector(), ifelse(is.null(color), activity_id(eventlog), color), units) +
		facet_grid(as.formula(paste(c(paste(groups, collapse = "+"), "~." ), collapse = "")), scales = "free_y", space = "free")

}


#' @rdname dotted_chart
#' @export idotted_chart

idotted_chart <- function(eventlog, plotly = FALSE) {

	ui <- miniPage(
		gadgetTitleBar("Interactive Dotted Chart"),
		miniContentPanel(
			column(width = 2,
				   selectizeInput("x", "x-axis:", choices = c("relative","relative_week","relative_day","absolute"), selected = "absolute"),
				   selectizeInput("sort", "Sort by:", choices = c("start","end","duration", "start_day","start_week"), selected = "start"),
				   selectizeInput("units", "Time units:", choices = c("secs","min","hours","days","weeks"), selected = "hours"),
				   selectizeInput("color", "Color:", choices = c(NA,NULL,colnames(eventlog)), selected = activity_id(eventlog))
			),
			column(width = 10,
				   uiOutput("plot")
			)
		)
	)

	server <- function(input, output, session){


		output$plot <- renderUI({
			if(plotly){
				plotlyOutput("plotly_dotted_chart", height = 700)
			} else {
				plotOutput("plot_dotted_chart", height = 700)
			}

		})

		output$plot_dotted_chart <- renderPlot({
			eventlog %>%
				dotted_chart(x = input$x,
							 sort = input$sort,
							 color = input$color,
							 units = input$units)
		})

		output$plotly_dotted_chart <- renderPlotly({
			eventlog %>%
				dotted_chart(x = input$x,
							 sort = input$sort,
							 color = input$color,
							 units = input$units) %>%
				ggplotly()
		})

		observeEvent(input$done, {
			stopApp()
		})
	}

	runGadget(shinyApp(ui, server), viewer = dialogViewer("Interactive Dotted Chart", height = 900, width = 1200))

}

#' @rdname dotted_chart
#' @export iplotly_dotted_chart

iplotly_dotted_chart <- function(eventlog) {
	idotted_chart(eventlog, plotly = TRUE)
}


#' @rdname dotted_chart
#' @export plotly_dotted_chart

plotly_dotted_chart <- function(eventlog) {
	dotted_chart(eventlog) %>%
		ggplotly
}
