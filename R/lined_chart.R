#' @title Lined chart
#' @description Create a lined chart to view all cases at a glance
#' @param eventlog Eventlog object
#' @param color Optional. Should be a case attribute! No coloring applied by default.
#' @param plotly Return plotly object
#' @param ... Deprecated arguments
#' @export lined_chart
#'


lined_chart <- function(eventlog, color, plotly,  ...) {
	UseMethod("lined_chart")
}


# compute data for dotted_chart
#' @export
lined_chart.eventlog <- function(eventlog, color = NULL, plotly, ...) {

	if(is.null(color)) {
		eventlog %>%
			group_by_case() -> eventlog
	} else {
		eventlog %>% group_by( !!case_id_(eventlog), !!sym(color)) %>% summarize() %>% summarize(n = n()) %>% filter(n > 1) -> filter_color

		if(nrow(filter_color) > 0) {
			stop("Attribute given to color argument is not a case attribute")
		} else {
			eventlog %>% group_by(!!sym(color),
								  !!case_id_(eventlog)) -> eventlog
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

	return(p)

}

col_vector <- function() {
	qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
	unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
}

#' @export


lined_chart.grouped_eventlog <- function(eventlog,
										  color = NULL,
										  ...) {


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
