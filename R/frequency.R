#' @title Frequency map profile
#' @description Function to create a frequency profile for a process map.
#' @param value The type of frequency value to be used:
#' absolute, relative (percentage of activity instances) or relative_case (percentage of cases the activity occurs in).
#' @param color_scale Name of color scale to be used for nodes. Defaults to PuBu. See `Rcolorbrewer::brewer.pal.info()` for all options.
#' @param color_edges The color used for edges. Defaults to dodgerblue4.
#' @export frequency


frequency <- function(value = c("absolute", "relative", "absolute_case", "relative_case"), color_scale = "PuBu", color_edges = "dodgerblue4") {
	value <- match.arg(value)
	attr(value, "perspective") <- "frequency"
	attr(value, "color") <- color_scale
	attr(value, "color_edges") <- color_edges


	attr(value, "create_nodes") <- function(precedence, type, extra_data) {

		n_cases <- extra_data$n_cases
		n_activity_instances <- extra_data$n_activity_instances


		precedence %>%
			group_by(ACTIVITY_CLASSIFIER_, from_id) %>%
			summarize(n = as.double(n()),
					  n_distinct_cases = as.double(n_distinct(CASE_CLASSIFIER_))) %>%
			ungroup() %>%
			mutate(label = case_when(type == "relative" ~ 100*n/n_activity_instances,
									 type == "absolute" ~ n,
									 type == "absolute_case" ~ n_distinct_cases,
									 type == "relative_case" ~ 100*n_distinct_cases/n_cases)) %>%
			mutate(color_level = label,
				   shape = if_end(ACTIVITY_CLASSIFIER_,"circle","rectangle"),
				   fontcolor = if_end(ACTIVITY_CLASSIFIER_, if_start(ACTIVITY_CLASSIFIER_, "chartreuse4","brown4"),  ifelse(label <= (min(label) + (5/8)*diff(range(label))), "black","white")),
				   color = if_end(ACTIVITY_CLASSIFIER_, if_start(ACTIVITY_CLASSIFIER_, "chartreuse4","brown4"),"grey"),
				   tooltip = paste0(ACTIVITY_CLASSIFIER_, "\n", round(label, 2), ifelse(type %in% c("absolute", "absolute_case"),"", "%")),
				   label = if_end(ACTIVITY_CLASSIFIER_, ACTIVITY_CLASSIFIER_, tooltip)) %>%
			na.omit()
	}

	attr(value, "create_edges") <- function(precedence, type, extra_data) {

		n_cases <- extra_data$n_cases
		n_activity_instances <- extra_data$n_activity_instances

		precedence %>%
			ungroup() %>%
			group_by(ACTIVITY_CLASSIFIER_, from_id, next_act, to_id) %>%
			summarize(n = as.double(n()),
					  n_distinct_cases = as.double(n_distinct(CASE_CLASSIFIER_))) %>%
			na.omit() %>%
			group_by(ACTIVITY_CLASSIFIER_, from_id) %>%
			mutate(label = case_when(type == "relative" ~ round(100*n/sum(n),2),
									 type == "absolute" ~ n,
									 type == "absolute_case" ~ n_distinct_cases,
									 type == "relative_case" ~ round(100*n_distinct_cases/n_cases, 2))) %>%
			ungroup() %>%
			mutate(penwidth = rescale(label, to = c(1,5), from = c(0, max(label)))) %>%
			mutate(label = case_when(type == "absolute"        ~ paste0(label, ""),
									 type == "absolute_case"  ~ paste0(label, ""),
									 type == "relative"        ~ paste0(label, "%"),
									 type == "relative_case"   ~ paste0(label, "%")))

	}
	return(value)
}

