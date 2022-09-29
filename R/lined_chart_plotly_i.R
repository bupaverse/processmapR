#' @include lined_chart.R
NULL

#' @keywords internal
#' @rdname deprecated
#' @export ilined_chart
ilined_chart <- function(eventlog, plotly = FALSE) {

	lifecycle::deprecate_warn("0.5.2", "ilined_chart()")

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

#' @keywords internal
#' @rdname deprecated
#' @export plotly_lined_chart
iplotly_lined_chart <- function(eventlog) {

	lifecycle::deprecate_warn("0.5.2", "iplotly_lined_chart()")

	ilined_chart(eventlog, plotly = TRUE)
}


#' @describeIn lined_chart [`plotly`] lined chart for [`log`][`bupaR::log`] objects.
#' @export plotly_lined_chart
plotly_lined_chart <- function(log,
															 x = c("absolute","relative"),
															 sort = c("auto","start","end","duration"),
															 color = NULL,
															 units = c("auto","secs","mins","hours","days","weeks"),
															 line_width = 2,
															 plotly = TRUE,
															 scale_color = bupaR::scale_color_discrete_bupaR,
															 eventlog = deprecated()) {

	lifecycle::deprecate_warn(when = "0.5.2",
														what = "plotly_lined_chart()",
														details = "Please use `lined_chart(..., plotly = TRUE)` instead.")

	log <- lifecycle_warning_eventlog(log, eventlog)

	lined_chart(log, x, sort, color, units, line_width, plotly, scale_color)
}
