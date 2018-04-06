
#' @title Resource Matrix
#'
#' @description  Construct a resource matrix, showing how work is handed over
#'
#' @param eventlog The event log object to be used
#' @param type The type of resource matrix, which can be absolulte, relative, relative_antecedent or relative_consequent. Absolute will return
#' a matrix with absolute frequencies, relative will return global relative frequencies for all antecedent-consequent pairs.
#' Relative_antecedent will return relative frequencies within each antecendent, i.e. showing the relative proportion of consequents within each antecedent. Relative_consequent will do the reverse.
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(patients)
#' precedence_matrix(patients)
#' }
#'
#' @export resource_matrix

resource_matrix <- function(eventlog, type = c("absolute","relative","relative_antecedent","relative_consequent")) {

	stopifnot("eventlog" %in% class(eventlog))
	min_order <- NULL

	type <- match.arg(type)
	log <- eventlog
	ts <- NULL
	timestamp_classifier <- NULL
	antecedent <- NULL
	consequent <- NULL
	aid <- NULL
	resource_classifier <- NULL
	case_classifier <- NULL


	colnames(log)[colnames(log) == resource_id(eventlog)] <- "resource_classifier"
	colnames(log)[colnames(log) == case_id(eventlog)] <- "case_classifier"
	colnames(log)[colnames(log) == timestamp(eventlog)] <- "timestamp_classifier"
	colnames(log)[colnames(log) == activity_instance_id(eventlog)] <- "aid"



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
		class(log) <- c("precedence_matrix", class(log))
		attr(log, "matrix_type") <- "absolute"
	}
	else if (type == "relative") {
		log %>%
			mutate(rel_n = n/sum(n)) -> log
		class(log) <- c("precedence_matrix", class(log))
		attr(log, "matrix_type") <- "relative"
	}
	else if (type == "relative_antecedent") {
		log %>%
			group_by(antecedent) %>%
			mutate(rel_antecedent = n/sum(n)) %>%
			ungroup() -> log
		class(log) <- c("precedence_matrix", class(log))
		attr(log, "matrix_type") <- "relative_antecedent"
	}
	else if(type == "relative_consequent") {
		log %>%
			group_by(consequent) %>%
			mutate(rel_consequent = n/sum(n)) %>%
			ungroup() -> log
		class(log) <- c("precedence_matrix", class(log))
		attr(log, "matrix_type") <- "relative_consequent"
	} else {
		stop("Argument type should be one of: absolute, relative, relative_antecedent, relative_consequent")
	}

	return(log)
}
