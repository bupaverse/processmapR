
#' @title Resource Matrix
#'
#' @description  Construct a resource matrix, showing how work is handed over
#'
#' @param type The type of resource matrix, which can be absolulte, relative, relative_antecedent or relative_consequent. Absolute will return
#' a matrix with absolute frequencies, relative will return global relative frequencies for all antecedent-consequent pairs.
#' Relative_antecedent will return relative frequencies within each antecendent, i.e. showing the relative proportion of consequents within each antecedent. Relative_consequent will do the reverse.
#' @inheritParams dotted_chart

#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(patients)
#' precedence_matrix(patients)
#' }
#'
#' @export

resource_matrix <- function(log, type, eventlog = deprecated()) {
	UseMethod("resource_matrix")
}

#' @describeIn resource_matrix Resource matrix of event log
#' @export

resource_matrix.eventlog <- function(log, type = c("absolute","relative","relative-antecedent","relative-consequent"), eventlog = deprecated()) {



	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn("0.4.0",
								  "resource_matrix(eventlog)",
								  "resource_matrix(log)")
		log <- eventlog
	}

	min_order <- NULL

	type <- match.arg(type)
	log
	ts <- NULL
	timestamp_classifier <- NULL
	antecedent <- NULL
	consequent <- NULL
	aid <- NULL
	resource_classifier <- NULL
	case_classifier <- NULL


	colnames(log)[colnames(log) == resource_id(log)] <- "resource_classifier"
	colnames(log)[colnames(log) == case_id(log)] <- "case_classifier"
	colnames(log)[colnames(log) == timestamp(log)] <- "timestamp_classifier"
	colnames(log)[colnames(log) == activity_instance_id(log)] <- "aid"



	log %>%
		group_by(case_classifier, resource_classifier, aid) %>%
		summarize(ts = min(timestamp_classifier), min_order = min(.order))  %>%
		group_by(case_classifier) %>%
		arrange(ts, min_order) %>%
		mutate(antecedent = resource_classifier,
			   consequent = lead(resource_classifier)) %>%
		ungroup() %>%
		select(antecedent, consequent) %>%
		na.omit() %>%
		count(antecedent, consequent) %>%
		ungroup() -> log

	if(type == "absolute") {
		class(log) <- c("process_matrix", class(log))
		attr(log, "matrix_type") <- "absolute"
	}
	else if (type == "relative") {
		log %>%
			mutate(rel_n = n/sum(n)) -> log
		class(log) <- c("process_matrix", class(log))
		attr(log, "matrix_type") <- "relative"
	}
	else if (type == "relative-antecedent") {
		log %>%
			group_by(antecedent) %>%
			mutate(rel_antecedent = n/sum(n)) %>%
			ungroup() -> log
		class(log) <- c("process_matrix", class(log))
		attr(log, "matrix_type") <- "relative-antecedent"
	}
	else if(type == "relative-consequent") {
		log %>%
			group_by(consequent) %>%
			mutate(rel_consequent = n/sum(n)) %>%
			ungroup() -> log
		class(log) <- c("process_matrix", class(log))
		attr(log, "matrix_type") <- "relative-consequent"
	} else {
		stop("Argument type should be one of: absolute, relative, relative-antecedent, relative-consequent")
	}
	attr(type, "perspective") <- "frequency"
	attr(log, "matrix_type") <- type

	return(log)
}

#' @describeIn resource_matrix Resource matrix of activity log
#' @export
#'
resource_matrix.activitylog <- function(log, type = c("absolute","relative","relative-antecedent","relative-consequent"), eventlog = deprecated()) {
	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn("0.4.0",
								  "resource_matrix(eventlog)",
								  "resource_matrix(log)")
		log <- eventlog
	}

	log %>% bupaR::to_eventlog() %>% resource_matrix(type)

}


