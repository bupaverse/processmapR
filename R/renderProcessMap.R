#' @title Widget render function for use in Shiny
#' @param expr an expression that generates a DiagrammeR graph.
#' @param env the environment in which to evaluate expr.
#' @param quoted is expr a quoted expression (with quote())?
#' This is useful if you want to save an expression in a variable.
#' @name renderProcessMap
#' @export

renderProcessMap <- function(expr, env = parent.frame(), quoted = FALSE) {
	if (!quoted) expr <- substitute(expr)

	DiagrammeR::renderGrViz(expr, env, quoted = TRUE)
}
