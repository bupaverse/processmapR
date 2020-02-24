
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

	antecedent <- NULL
	consequent <- NULL
	ts <- NULL
	min_order <- NULL


	if (type == "relative-case") {

		# This is the slow case

		eventlog %>%
			as.data.frame() %>%
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
			select(!!case_id_(eventlog), antecedent, consequent) %>%
			na.omit() %>%
			group_by(antecedent, consequent) %>%
			summarize(n_cases = n_distinct(!!case_id_(eventlog))) %>%
			mutate(rel_n_cases = n_cases/n_cases(eventlog)) %>%
			ungroup() -> m

		n_consequents <- length(unique(m$consequent))

		m %>%
			mutate(antecedent = fct_relevel(antecedent, "Start"),
				   consequent = fct_relevel(consequent, "End", after = n_consequents - 1)) -> m


	} else {

		# Use Rcpp for the rest

		m <- precedence_matrix_absolute(eventlog)

		if (type == "absolute") {
			# nothing
		} else if (type == "relative") {
			m %>%
				mutate(rel_n = n / sum(n)) -> m
		}
		else if (type == "relative-antecedent") {
			m %>%
				group_by(antecedent) %>%
				mutate(rel_antecedent = n / sum(n)) %>%
				ungroup() -> m
		}
		else if (type == "relative-consequent") {
			m %>%
				group_by(consequent) %>%
				mutate(rel_consequent = n / sum(n)) %>%
				ungroup() -> m
		} else {
			stop(paste0("Unknown type ", type))
		}
	}

	class(m) <- c("process_matrix", class(m))
	attr(type, "perspective") <- "frequency"
	attr(m, "matrix_type") <- type

	return(m)
}


#' Precedence Matrix
#'
#' Construct a precedence matrix, showing how activities are followed by each other.
#' This function computes the precedence matrix directly in C++ for efficiency.
#' Only the type `absolute` of (\code{\link[processmapR]{precedence_matrix}}) is supported.
#'
#' @param eventlog The event log object to be used.
#' @param lead The distance between activities following/preceding each other.
#'
#' @examples
#' library(eventdataR)
#' data(traffic_fines)
#' m <- precedence_matrix_absolute(traffic_fines)
#' print(m)
#' as.matrix(m)
#'
#' @export precedence_matrix_absolute
precedence_matrix_absolute <- function(eventlog, lead = 1) {
  stopifnot("eventlog" %in% class(eventlog))
  stopifnot(lead > 0)

  eventlog <- reduce_simple_eventlog(eventlog)
  precedence_matrix_absolute_impl(eventlog, lead)
}

precedence_matrix_absolute_impl <- function(simplelog, lead = 1) {
  mat <- as_tibble(count_precedence(simplelog$case_id,
                                    simplelog$activity_id,
                                    lead))

  class(mat) <- c("process_matrix", class(mat))
  type <- "absolute"
  attr(type, "perspective") <- "frequency"
  attr(mat, "matrix_type") <- type
  return(mat)
}

reduce_simple_eventlog <- function(eventlog) {
  .order <- NULL

  eventlog %>%
      as.data.frame() %>%
      droplevels() %>%
      arrange(!!case_id_(eventlog), !!timestamp_(eventlog), .order) %>%
      # relies on dplyr taking the first distinct value
      distinct(!!case_id_(eventlog), !!activity_id_(eventlog), !!activity_instance_id_(eventlog)) %>%
      rename(case_id = !!case_id_(eventlog),
             activity_id = !!activity_id_(eventlog),
             activity_instance_id = !!activity_instance_id_(eventlog))
}
