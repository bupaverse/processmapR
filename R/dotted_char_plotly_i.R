#' @include dotted_chart.R
NULL

#' @keywords internal
#' @rdname deprecated
#' @export idotted_chart
idotted_chart <- function(eventlog, plotly = FALSE) {

  lifecycle::deprecate_warn("0.5.2", "idotted_chart()")

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

#' @keywords internal
#' @rdname deprecated
#' @export iplotly_dotted_chart
iplotly_dotted_chart <- function(eventlog) {

  lifecycle::deprecate_warn("0.5.2", "iplotly_dotted_chart()")

  idotted_chart(eventlog, plotly = TRUE)
}

#' @keywords internal
#' @rdname deprecated
#' @export plotly_dotted_chart
plotly_dotted_chart <- function(log,
                                x = c("absolute","relative","relative_week","relative_day"),
                                sort = c("auto","start","end","duration","start_week","start_day"),
                                color = NULL,
                                units = c("auto","secs","mins","hours","days","weeks"),
                                add_end_events = FALSE,
                                scale_color = bupaR::scale_color_discrete_bupaR,
                                eventlog = deprecated()) {

  lifecycle::deprecate_warn(when = "0.5.2",
                            what = "plotly_dotted_chart()",
                            details = "Please use `dotted_chart(..., plotly = TRUE)` instead.")

  log <- lifecycle_warning_eventlog(log, eventlog)

  log %>%
    dotted_chart(x, sort, color, units, add_end_events, scale_color, TRUE)
}
