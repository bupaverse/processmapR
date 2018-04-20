
#' @title Resource Map
#'
#'
#' @description A function for creating a resource map of an event log based on handover of work.
#' @param eventlog The event log object for which to create a resource map
#' @param type A process map type, which can be created with the functions frequency and performance. The first type focusses on the frequency aspect of a process, while the second one focussed on processing time.
#' @param render Whether the map should be rendered immediately (default), or rather an object of type dgr_graph should be returned.
#' @param ... Deprecated arguments
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(patients)
#' resource_map(patients)
#' }
#' @export resource_map




resource_map <- function(eventlog, type = frequency("absolute"), render = T, ...) {
	event_classifier <- activity_id(eventlog)
	resource_classifier <- resource_id(eventlog)

	n <- eventlog %>%
		names %>%
		str_replace_all(paste0("^",event_classifier, "$"), "old_event_classifier") %>%
		str_replace_all(paste0("^",resource_classifier, "$"), event_classifier) %>%
		str_replace_all("old_event_classifier",resource_classifier)

	eventlog %>%
		set_names(n) %>%
		process_map(render = render, type = type, ...)


}
