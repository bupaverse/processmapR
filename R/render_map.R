
#' Render process map
#'
#' @inheritParams DiagrammeR::render_graph
#' @param map A `process_map` created with [`process_map`][`processmapR::process_map`] and argument `render = F`.
#'
#' @export
#'
render_map <- function(map,
					   layout = NULL,
					   output = NULL,
					   as_svg = FALSE,
					   title = NULL,
					   width = NULL,
					   height = NULL) {

	DiagrammeR::render_graph(graph = map,
							 output = output,
							 as_svg = as_svg,
							 title = title,
							 width = width,
							 height = height
	)
}
