
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
