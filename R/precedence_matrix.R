
#' @title Precendence Matrix
#'
#' @description  Construct a precendence matrix, showing how activities are followed by each other.
#'
#' @param eventlog The event log object to be used
#' @param type The type of precedence matrix, which can be absolulte, relative, relative-antecedent or relative-consequent. Absolute will return
#' a matrix with absolute frequencies, relative will return global relative frequencies for all antecedent-consequent pairs.
#' Relative-antecedent will return relative frequencies within each antecendent, i.e. showing the relative proportion of consequents within each antecedent.
#' Relative-consequent will do the reverse.
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(patients)
#' precedence_matrix(patients)
#' }
#'
#' @export precedence_matrix

precedence_matrix <- function(eventlog, type = c("absolute","relative","relative-antecedent","relative-consequent", "relative-case")) {
	stopifnot("eventlog" %in% class(eventlog))

	type <- match.arg(type)

	if(type == "relative_antecedent") {
		type <- "relative-antecedent"
		warning("Use type = 'relative-antecedent' instead of 'relative_antecedent'")
	}
	if(type == "relative_consequent") {
		type <- "relative-consequent"
		warning("Use type = 'relative-consequent' instead of 'relative_consequent'")
	}
	if(type == "relative_case") {
		type <- "relative-case"
		warning("Use type = 'relative-case' instead of 'relative_case'")
	}

	type <- match.arg(type)
	log <- eventlog
	aid <- NULL
	case_classifier <- NULL
	event_classifier <- NULL
	timestamp_classifier <- NULL
	antecedent <- NULL
	consequent <- NULL
	ts <- NULL
	min_order <- NULL

	log %>%
		group_by(!!case_id_(eventlog), !!activity_id_(eventlog), !!activity_instance_id_(eventlog)) %>%
		summarize(ts = min(!!timestamp_(eventlog)), min_order = min(.order))  %>%
		group_by(!!case_id_(eventlog)) %>%
		arrange(ts, min_order) %>%
		mutate(antecedent = as.character(!!activity_id_(eventlog)),
				  consequent = lead(as.character(!!activity_id_(eventlog)), default = "End")) -> temp

	temp %>%
		arrange(ts, min_order) %>%
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
		;
	}
	else if (type == "relative") {
		log %>%
			mutate(rel_n = n/sum(n)) -> log

	}
	else if (type == "relative-antecedent") {
		log %>%
			group_by(antecedent) %>%
			mutate(rel_antecedent = n/sum(n)) %>%
			ungroup() -> log

	}
	else if(type == "relative-consequent") {
		log %>%
			group_by(consequent) %>%
			mutate(rel_consequent = n/sum(n)) %>%
			ungroup() -> log

	}
	else if (type == "relative-case") {
		temp %>%
			arrange(ts, min_order) %>%
			slice(1:1) %>%
			mutate(consequent = antecedent,
				   antecedent = "Start") %>%
			bind_rows(temp) %>%
			ungroup() %>%
			select(!!case_id_(eventlog), antecedent, consequent) %>%
			na.omit() %>%
			group_by(antecedent, consequent) %>%
			summarize(n_cases = n_distinct(!!case_id_(eventlog))) %>%
			mutate(rel_n_cases = n_cases/n_cases(eventlog)) %>%
			ungroup() -> log

		n_consequents <- length(unique(log$consequent))

		log %>%
			mutate(antecedent = fct_relevel(antecedent, "Start"),
				   consequent = fct_relevel(consequent, "End", after = n_consequents - 1)) -> log

	}
	class(log) <- c("process_matrix", class(log))
	attr(type, "perspective") <- "frequency"
	attr(log, "matrix_type") <- type


	return(log)
}
