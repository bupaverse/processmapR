#' @title Dotted chart
#' @description Create a dotted chart to view all events in a glance
#' @param eventlog Eventlog object
#' @param x Value for plot on x-axis: absolute time or relative time (since start, since start of week, since start of day)
#' @param y Ordering of the cases on y-axis: start, end or duration
#' @param color Optional, variable to use for coloring dots. Default is the activity identifier. Use NA for no colors.
#' @param units Time units to use on x-axis in case of relative time.
#'
#' @export dotted_chart
dotted_chart <- function(eventlog,
						 x = c("absolute","relative","relative_week","relative_day"),
						 y = c("start","end","duration"),
						 color = NULL,
						 units = c("weeks","days","hours","mins","secs")) {


	#Utility functions for week/day caluclations
	timeSinceStartOfWeek <- function(time) {
		midnight <- trunc(time, "days")
		weekDay <- as.integer(format(time, "%w"))
		weekDay <- ifelse(weekDay, weekDay-1, 6) # Let week start with Monday
		msSinceMidnight <- difftime(time, midnight, unit="secs")
		as.difftime(msSinceMidnight + weekDay*24*60*60, units = "secs")
	}
	timeSinceStartOfDay <- function(time) {
		midnight <- trunc(time, "days")
		difftime(time, midnight, unit="secs")
	}

	x <- match.arg(x)
	y <- match.arg(y)
	units <- match.arg(units)

	if("grouped_eventlog" %in% class(eventlog))
		groups <- groups(eventlog)
	else
		groups <- NULL

	color_flag <- T
	if(is.null(color)) {
		color <- activity_id(eventlog)
	} else if(is.na(color)) {
		color_flag <- F
		color <- quo("undefined")
	}


	if("grouped_eventlog" %in% class(eventlog)) {
		eventlog %>%
			rename_("ts" = timestamp(eventlog),
					"case_classifier" = case_id(eventlog)) %>%
			mutate_("color" = color) %>%
			group_by(case_classifier,!!as.symbol(activity_id(eventlog)),!!as.symbol(activity_instance_id(eventlog)), color, add = T) %>%
			summarize(start = min(ts),
					  end = max(ts)) %>%
			group_by(case_classifier) %>%
			mutate(start_week = as.double(timeSinceStartOfWeek(start), units = units),
				   end_week = as.double(timeSinceStartOfWeek(end), units = units)) %>%
			mutate(start_day = as.double(timeSinceStartOfDay(start), units = units),
				   end_day = as.double(timeSinceStartOfDay(end), units = units)) %>%
			mutate(start_case = min(start),
				   end_case = max(end),
				   dur = as.double(end_case - start_case, units = units)) %>%
			# add the time since week/day start for the first/last event in the case
			mutate(start_case_week = timeSinceStartOfWeek(start_case),
				   end_case_week = timeSinceStartOfWeek(end_case),
				   start_case_day = timeSinceStartOfDay(start_case),
				   end_case_day = timeSinceStartOfDay(end_case)) %>%
			mutate(start_relative = as.double(start - start_case, units = units),
				   end_relative = end - start_case) -> data
	} else {
		eventlog %>%
			rename_("ts" = timestamp(eventlog),
					"case_classifier" = case_id(eventlog)) %>%
			mutate_("color" = color) %>%
			group_by(case_classifier,!!as.symbol(activity_id(eventlog)),!!as.symbol(activity_instance_id(eventlog)), color) %>%
			summarize(start = min(ts),
					  end = max(ts)) %>%
			group_by(case_classifier) %>%
			mutate(start_week = as.double(timeSinceStartOfWeek(start), units = units),
				   end_week = as.double(timeSinceStartOfWeek(end), units = units)) %>%
			mutate(start_day = as.double(timeSinceStartOfDay(start), units = units),
				   end_day = as.double(timeSinceStartOfDay(end), units = units)) %>%
			mutate(start_case = min(start),
				   end_case = max(end),
				   dur = as.double(end_case - start_case, units = units)) %>%
			# add the time since week/day start for the first/last event in the case
			mutate(start_case_week = timeSinceStartOfWeek(start_case),
				   end_case_week = timeSinceStartOfWeek(end_case),
				   start_case_day = timeSinceStartOfDay(start_case),
				   end_case_day = timeSinceStartOfDay(end_case)) %>%
			mutate(start_relative = as.double(start - start_case, units = units),
				   end_relative = end - start_case) -> data
	}

	if(x == "absolute") {
		if(y == "start") {
			data %>%
				ggplot(aes(x = start, y = reorder(case_classifier, desc(start_case)))) -> p
		} else if (y == "end") {
			data %>%
				ggplot(aes(x = start, y = reorder(case_classifier, desc(end_case))))  -> p
		} else if (y == "duration") {
			data %>%
				ggplot(aes(x = start, y = reorder(case_classifier, desc(dur))))  -> p
		}
	} else if(x == "relative") {
		if(y == "start") {
			data %>%
				ggplot(aes(x = start_relative, y = reorder(case_classifier, desc(start_case)))) -> p
		} else if (y == "end") {
			data %>%
				ggplot(aes(x = start_relative, y = reorder(case_classifier, desc(end_case))))  -> p
		} else if (y == "duration") {
			data %>%
				ggplot(aes(x = start_relative, y = reorder(case_classifier, desc(dur)))) -> p
		}
	} else if (x == "relative_week") {
		if(y == "start") {
			data %>%
				ggplot(aes(x = start_week, y = reorder(case_classifier, desc(start_case_week)))) -> p
		} else if (y == "end") {
			data %>%
				ggplot(aes(x = start_week, y = reorder(case_classifier, desc(end_case_week))))  -> p
		} else if (y == "duration") {
			data %>%
				ggplot(aes(x = start_week, y = reorder(case_classifier, desc(dur)))) -> p
		}
	} else if (x == "relative_day") {
		if(y == "start") {
			data %>%
				ggplot(aes(x = start_day, y = reorder(case_classifier, desc(start_case_day)))) -> p
		} else if (y == "end") {
			data %>%
				ggplot(aes(x = start_day, y = reorder(case_classifier, desc(end_case_day))))  -> p
		} else if (y == "duration") {
			data %>%
				ggplot(aes(x = start_day, y = reorder(case_classifier, desc(dur)))) -> p
		}
	}

	p + scale_y_discrete(breaks = NULL) -> p


	if(color_flag) {
		p + geom_point(aes(color = color)) + scale_color_brewer(name = color, palette = "Spectral") -> p
	} else {
		p + geom_point(color = "grey") -> p
	}

	# time formatter for the week and day options
	timeFormat <- function(time){
		substr(format(as.hms(as.double(time, units = "secs") %% (24 * 60 * 60))),0,5)
	}

	if(x == "relative") {
		p + scale_x_continuous() + labs(x = glue::glue("Time since start case (in {units})")) -> p
	} else if (x == "relative_week") {
		if (units == "secs") {
			p + scale_x_time(breaks = as.hms(seq(0, 7 * 86400, by = 8 * 3600)), labels = timeFormat) +
				geom_vline(xintercept = seq(0, 7 * 86400, by = 86400), colour="black") +
				labs(x = glue::glue("Time since start of week/monday")) -> p
		} else {
			p + scale_x_continuous() + labs(x = glue::glue("Time since start of week/monday (in {units})")) -> p
		}
	} else if (x == "relative_day") {
		if (units == "secs") {
			p + scale_x_time(breaks = as.hms(seq(0, 86400, by = 2 * 3600))) + labs(x = glue::glue("Time since start of day")) -> p
		} else {
			p + scale_x_continuous() + labs(x = glue::glue("Time since start of day (in {units})")) -> p
		}
	} else {
		p + labs(x = "Time") -> p
	}

	if(!is.null(groups)) {
		p <- p + facet_grid(as.formula(paste(c(paste(groups, collapse = "+"), "~." ), collapse = "")), scales = "free_y", space = "free")
	}

	p + labs(y = "Cases") +
		theme_light()
}


#' @rdname dotted_chart
#' @export idotted_chart

idotted_chart <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Interactive Dotted Chart"),
		miniContentPanel(
			column(width = 2,
				   selectizeInput("x", "x-axis:", choices = c("relative","relative_week","relative_day","absolute"), selected = "absolute"),
				   selectizeInput("y", "y-axis order:", choices = c("start","end","duration"), selected = "start"),
				   selectizeInput("units", "Time units:", choices = c("secs","min","hours","days","weeks"), selected = "hours"),
				   selectizeInput("color", "Color:", choices = c(NA,NULL,colnames(eventlog)), selected = "event")
			),
			column(width = 10,
				   plotOutput("dotted_chart")
			)
		)
	)



	server <- function(input, output, session){
		output$dotted_chart <- renderPlot({
			eventlog %>%
				dotted_chart(x = input$x,
							 y = input$y,
							 color = input$color,
							 units = input$units)
		})

		observeEvent(input$done, {
			stopApp()
		})
	}

	runGadget(shinyApp(ui, server), viewer = dialogViewer("Interactive Dotted Chart", height = 900, width = 1200))

}
