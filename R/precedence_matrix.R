
#' @title Precendence Matrix
#'
#' @description  Construct a precendence matrix, showing how activities are followed by each other.
#'
#' @param eventlog The event log object to be used
#' @param type The type of precedence matrix, which can be absolulte, relative, relative_antecedent or relative_consequent. Absolute will return
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
#' @export precedence_matrix

precedence_matrix <- function(eventlog, type = c("absolute","relative","relative_antecedent","relative_consequent")) {
	stopifnot("eventlog" %in% class(eventlog))

	type <- match.arg(type)
	log <- eventlog
	aid <- NULL
	case_classifier <- NULL
	event_classifier <- NULL
	timestamp_classifier <- NULL
	antecedent <- NULL
	consequent <- NULL
	ts <- NULL

	colnames(log)[colnames(log) == activity_id(eventlog)] <- "event_classifier"
	colnames(log)[colnames(log) == case_id(eventlog)] <- "case_classifier"
	colnames(log)[colnames(log) == timestamp(eventlog)] <- "timestamp_classifier"
	colnames(log)[colnames(log) == activity_instance_id(eventlog)] <- "aid"



	log %>%
		group_by(case_classifier, event_classifier, aid) %>%
		summarize(ts = min(timestamp_classifier))  %>%
		group_by(case_classifier) %>%
		arrange(ts) %>%
		mutate(antecedent = event_classifier,
				  consequent = lead(event_classifier, default = "End")) -> temp

	temp %>%
		arrange(ts) %>%
		slice(1:1) %>%
		mutate(consequent = antecedent,
			   antecedent = "Start") %>%
		bind_rows(temp) %>%
		ungroup() %>%
		select(antecedent, consequent) %>%
		na.omit() %>%
		count(antecedent, consequent) %>%
		ungroup() -> log

	n_consequents <- length(unique(log$consequent))

	log %>%
		mutate(antecedent = fct_relevel(antecedent, "Start"),
			   consequent = fct_relevel(consequent, "End", after = n_consequents - 1)) -> log

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
