#' @title Widget output function for use in Shiny
#' @param outputId Output variable to read from.
#' @param width A valid CSS unit for the width or a number,
#' which will be coerced to a string and have px appended.
#' @param heigth A valid CSS unit for the height or a number,
#' which will be coerced to a string and have px appended.
#' @name processMapOutput
#' @importFrom DiagrammeR grVizOutput
#' @export

processMapOutput <- function(outputId, width = "100%", heigth = "400px") {

	DiagrammeR::grVizOutput(outputId, width, heigth)
}
