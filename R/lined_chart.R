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


lined_chart <- function(eventlog,
						x = c("absolute","relative"),
						sort = NULL,
						color,
						units,
						line_width = 2,
						plotly,
						...) {
	UseMethod("lined_chart")
}


# compute data for dotted_chart
#' @export
lined_chart.eventlog <- function(eventlog,
								 x = c("absolute","relative"),
								 sort = NULL,
								 color = NULL,
								 units = c("weeks","days","hours","mins","secs"),
								 line_width = 2,
					  		 	plotly = FALSE, ...) {

	x <- match.arg(x)
	units <- match.arg(units)
	mapping <- mapping(eventlog)

	if(is.null(sort)) {
		y <-	switch(x,
					"absolute" = "start",
					"relative" =  "duration")
	} else {
		y <-	match.arg(sort, choices = c("start","duration", "end"))
	}


	eventlog %>%
		lined_chart_data(color, units) %>%
		lined_chart_plot(mapping, x, y, col_vector(), ifelse(is.null(color), activity_id(eventlog), color), units, line_width = line_width)


}

col_vector <- function() {
	qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
	unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
}

lined_chart_data <- function(eventlog, color, units) {
	start_case_rank <- NULL
	start <- NULL
	end <- NULL
	start_case <- NULL
	end_case <- NULL

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
		as.data.frame() %>%
		group_by(!!case_id_(eventlog),!!activity_id_(eventlog),!!activity_instance_id_(eventlog), color, add = T) %>%
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
		full_join(eventlog_rank_start_cases)
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


#' @export


lined_chart.grouped_eventlog <- function(eventlog,
										 x = c("absolute","relative"),
										 sort = NULL,
										 color = NULL,
										 units = c("weeks","days","hours","mins","secs"),
										 line_width = 2,
										 plotly = FALSE, ...) {


	groups <- groups(eventlog)
	mapping <- mapping(eventlog)

	if(is.null(color)) {
		eventlog %>%
			group_by(!!case_id_(eventlog), add = TRUE) -> eventlog
	} else {
		eventlog %>% group_by( !!case_id_(eventlog), !!sym(color)) %>% summarize() %>% summarize(n = n()) %>% filter(n > 1) -> filter_color

		if(nrow(filter_color) > 0) {
			stop("Attribute given to color argument is not a case attribute")
		} else {
			eventlog %>% group_by(!!sym(color),
								  !!case_id_(eventlog), add = TRUE) -> eventlog
		}
	}


	eventlog %>%
		summarize(min = min(!!timestamp_(eventlog)), max = max(!!timestamp_(eventlog))) %>%
		ggplot(aes(x = min, xend = max, y = fct_reorder(!!case_id_(eventlog), desc(min)), yend = fct_reorder(!!case_id_(eventlog), desc(min)),
				   group = !!case_id_(eventlog))) +
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


#' @rdname lined_chart
#' @export ilined_chart

ilined_chart <- function(eventlog, plotly = FALSE) {

	ui <- miniPage(
		gadgetTitleBar("Interactive Lined Chart"),
		miniContentPanel(
			column(width = 2,
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
				plotlyOutput("plotly_lined_chart", height = 700)
			} else {
				plotOutput("plot_lined_chart", height = 700)
			}

		})

		output$plot_lined_chart <- renderPlot({
			eventlog %>%
				lined_chart(color = input$color)
		})

		output$plotly_lined_chart <- renderPlotly({
			eventlog %>%
				lined_chart(color = input$color) %>%
				ggplotly()
		})

		observeEvent(input$done, {
			stopApp()
		})
	}

	runGadget(shinyApp(ui, server), viewer = dialogViewer("Interactive Line Chart", height = 900, width = 1200))

}

#' @rdname lined_chart
#' @export plotly_lined_chart

iplotly_lined_chart <- function(eventlog) {
	ilined_chart(eventlog, plotly = TRUE)
}


#' @rdname lined_chart
#' @export plotly_lined_chart

plotly_lined_chart <- function(eventlog,
								color = NULL,
								...) {
	lined_chart(eventlog, color) %>%
		ggplotly
}
