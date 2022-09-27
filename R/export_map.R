
#' Export process map to pdf, png, ps or svg.
#'
#' @inheritParams export_graph
#' @param map A `process_map` created with [`process_map`][`processmapR::process_map`] and argument `render = F`.
#'
#' @export
#'
export_map <- function(map,
					   file_name = NULL,
					   file_type = NULL,
					   title = NULL,
					   width = NULL,
					   height = NULL) {

	DiagrammeR::export_graph(graph = map,
							 file_name = file_name,
							 file_type = file_type,
							 title = title,
							 width = width,
							 height = height
							 )
}
