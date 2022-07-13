
#' @title Resource Map
#' @description A function for creating a resource map of an event log based on handover of work.
#' @param type A process map type, which can be created with the functions frequency and performance. The first type focusses on the frequency aspect of a process, while the second one focussed on processing time.
#' @param render Whether the map should be rendered immediately (default), or rather an object of type dgr_graph should be returned.
#' @param ... Deprecated arguments
#' @inheritParams dotted_chart
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(patients)
#' resource_map(patients)
#' }
#' @export

resource_map <- function(log, type, render, ..., eventlog = deprecated()) {
	UseMethod("resource_map")
}

#' @describeIn resource_map Create resource map for eventlog
#' @export


resource_map.eventlog <- function(log, type = frequency("absolute"), render = T, ..., eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn("0.4.0",
								  "process_matrix(eventlog)",
								  "process_matrix(log)")
		log <- eventlog
	}

	event_classifier <- activity_id(log)
	resource_classifier <- resource_id(log)

	n <- log %>%
		names %>%
		str_replace_all(paste0("^",event_classifier, "$"), "old_event_classifier") %>%
		str_replace_all(paste0("^",resource_classifier, "$"), event_classifier) %>%
		str_replace_all("old_event_classifier",resource_classifier)

	log %>%
		set_names(n) %>%
		process_map(render = render, type = type, ...)
}


#' @describeIn resource_map Create resource map for activity log
#' @export

resource_map.activitylog <- function(log, type = frequency("absolute"), render = T, ..., eventlog = deprecated()) {
	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn("0.4.0",
								  "process_matrix(eventlog)",
								  "process_matrix(log)")
		log <- eventlog
	}

	log %>% bupaR::to_eventlog() %>% resource_map(type,render)



}

