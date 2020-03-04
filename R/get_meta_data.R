



#' Get data values for activities and flows from process map
#'
#' @param process_map An object created using process_map function. Can both be a rendered or not rendered object.
#'
#' @export
#'
#'
get_activities <- function(process_map) {
	attr(process_map, "nodes")
}
#' @rdname get_activities
#' @export
get_flows <- function(process_map) {
	attr(process_map, "edges")
}
