


#' Configure layout parameters for process map
#'
#' @param fixed_positions When specified as a data.frame with three columns 'act', 'x', and 'y' the position of nodes is fixed. Note that his can only be used with the 'neato' layout engine.
#' @param edge_weight When `TRUE` then the frequency with which an edge appears in the process map has influence on the process map layout. Edges with higher frequency get higher priority in the layout algorithm, which increases the visibility of 'process highways'.
#' @param edge_cutoff Edges that appear in the process map below this frequency are not considered at all when calculating the layout. This may create very long and complicated edge routings when choosen too high.
#'
#' @export
#'
layout_pm <- function(fixed_positions= NULL,
				   edge_weight = FALSE,
				   edge_cutoff = 0.0) {

	layout <- list()

	class(layout) <- c("process_map_layout", class(layout))

	layout$fixed_positions = fixed_positions
	layout$edge_weight = edge_weight
	layout$edge_cutoff = edge_cutoff
	return(layout)

}
