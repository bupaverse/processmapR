#' @title Frequency map profile
#' @description Function to create a frequency profile for a process map.
#' @param value The type of frequency value to be used:
#' absolute, relative (percentage of activity instances) or relative_case (percentage of cases the activity occurs in).
#' @param color_scale Name of color scale to be used for nodes. Defaults to PuBu. See `Rcolorbrewer::brewer.pal.info()` for all options.
#' @param color_edges The color used for edges. Defaults to dodgerblue4.
#' @export frequency


frequency <- function(value = c("absolute", "relative", "absolute-case", "relative-case", "relative-antecedent","relative-consequent"), color_scale = "PuBu", color_edges = "dodgerblue4") {

	value <- str_replace(value, "_", "-")
	value <- match.arg(value)
	attr(value, "perspective") <- "frequency"
	attr(value, "color") <- color_scale
	attr(value, "color_edges") <- color_edges


	attr(value, "create_nodes") <- function(precedence, type, extra_data) {

		n_cases <- extra_data$n_cases
		n_activity_instances <- extra_data$n_activity_instances
		from_id <- NULL
		to_id <- NULL
		label <- NULL
		tooltip <- NULL
		next_act <- NULL
		value <- NULL
		CASE_CLASSIFIER_ <- NULL

		ACTIVITY_CLASSIFIER_ <- NULL
		label_numeric <- NULL
		consequent <- NULL
	antecedent <- NULL
	n_distinct_cases <- NULL


		precedence %>%
			group_by(ACTIVITY_CLASSIFIER_, from_id) %>%
			summarize(n = as.double(n()),
					  n_distinct_cases = as.double(n_distinct(CASE_CLASSIFIER_))) %>%
			ungroup() %>%
			na.omit() %>%  # exclude invalid rows before computation
			mutate(label = case_when(type == "relative" ~ 100*n/n_activity_instances,
									 type == "absolute" ~ n,
									 type == "absolute-case" ~ n_distinct_cases,
									 type == "relative-case" ~ 100*n_distinct_cases/n_cases,
									 type == "relative-antecedent" ~ 100*n/n_activity_instances,
									 type == "relative-consequent" ~ 100*n/n_activity_instances)) %>%
			mutate(color_level = label,
				   value = label,
				   shape = if_end(ACTIVITY_CLASSIFIER_,"circle","rectangle"),
				   fontcolor = if_end(ACTIVITY_CLASSIFIER_, if_start(ACTIVITY_CLASSIFIER_, "chartreuse4","brown4"),  ifelse(label <= (min(label) + (5/8)*diff(range(label))), "black","white")),
				   color = if_end(ACTIVITY_CLASSIFIER_, if_start(ACTIVITY_CLASSIFIER_, "chartreuse4","brown4"),"grey"),
				   tooltip = paste0(ACTIVITY_CLASSIFIER_, "\n", round(label, 2), ifelse(type %in% c("absolute", "absolute-case"),"", "%")),
				   label = if_end(ACTIVITY_CLASSIFIER_, recode(ACTIVITY_CLASSIFIER_, ARTIFICIAL_START = "Start",ARTIFICIAL_END = "End"),
				   			   tooltip))
	}

	attr(value, "create_edges") <- function(precedence, type, extra_data) {
		from_id <- NULL
		to_id <- NULL
		label <- NULL
		tooltip <- NULL
		next_act <- NULL
		value <- NULL
		ACTIVITY_CLASSIFIER_ <- NULL
		label_numeric <- NULL
		consequent <- NULL
		CASE_CLASSIFIER_ <- NULL
		antecedent <- NULL
		n_distinct_cases <- NULL
		n_cases <- extra_data$n_cases
		n_activity_instances <- extra_data$n_activity_instances
		penwidth <- NULL

		if(!(type %in% c("relative-antecedent","relative-consequent"))) {

		precedence %>%
			ungroup() %>%
			group_by(ACTIVITY_CLASSIFIER_, from_id, next_act, to_id) %>%
			summarize(n = as.double(n()),
					  n_distinct_cases = as.double(n_distinct(CASE_CLASSIFIER_))) %>%
			na.omit() %>%
			group_by(ACTIVITY_CLASSIFIER_, from_id) %>%
			mutate(label_numeric = case_when(type == "relative" ~ n/sum(n),
											 type == "absolute" ~ n,
											 type == "absolute-case" ~ n_distinct_cases,
											 type == "relative-case" ~ n_distinct_cases/n_cases)) %>%
			ungroup() %>%
			mutate(penwidth = rescale(label_numeric, to = c(1,5), from = c(0, max(label_numeric)))) %>%
			mutate(label = case_when(type == "absolute"        ~ paste0(label_numeric, ""),
									 type == "absolute-case"  ~ paste0(label_numeric, ""),
									 type == "relative"        ~ paste0(round(100*label_numeric,2), "%"),
									 type == "relative-case"   ~ paste0(round(100*label_numeric,2), "%")))
		} else if (type == "relative-antecedent") {
			precedence %>%
				ungroup() %>%
				group_by(ACTIVITY_CLASSIFIER_, from_id, next_act, to_id) %>%
				summarize(n = as.double(n())) %>%
				na.omit() %>%
				group_by(ACTIVITY_CLASSIFIER_, from_id) %>%
				mutate(label_numeric = n/sum(n)) %>%
				ungroup() %>%
				mutate(penwidth = rescale(label_numeric, to = c(1,5), from = c(0, max(label_numeric)))) %>%
				mutate(label = paste0(round(100*label_numeric,2), "%"))
		} else {
			precedence %>%
				ungroup() %>%
				group_by(ACTIVITY_CLASSIFIER_, from_id, next_act, to_id) %>%
				summarize(n = as.double(n())) %>%
				na.omit() %>%
				group_by(next_act, to_id) %>%
				mutate(label_numeric = n/sum(n)) %>%
				ungroup() %>%
				mutate(penwidth = rescale(label_numeric, to = c(1,5), from = c(0, max(label_numeric)))) %>%
				mutate(label = paste0(round(100*label_numeric,2), "%"))

		}

	}

	attr(value, "transform_for_matrix") <- function(edges, type, extra_data) {
		from_id <- NULL
		to_id <- NULL
		label <- NULL
		tooltip <- NULL
		next_act <- NULL
		value <- NULL
		ACTIVITY_CLASSIFIER_ <- NULL
		label_numeric <- NULL
		n_distinct_cases <- NULL
		penwidth <- NULL
		consequent <- NULL
		n_consequents <- length(unique(edges$next_act))
		antecedent <- NULL
		edges %>%
			rename(antecedent = ACTIVITY_CLASSIFIER_,
				   consequent = next_act) %>%
			mutate(antecedent = fct_relevel(antecedent, "Start"),
				   consequent = fct_relevel(consequent, "End", after = n_consequents - 1)) -> edges

		edges %>%
			select(-from_id, -to_id, -n_distinct_cases, -label, -penwidth) -> edges

		if(type == "absolute") {
			edges %>%
				select(-label_numeric)
		} else if(type == "relative-case") {
			edges %>%
				mutate(n_cases = label_numeric*extra_data$n_cases) %>%
				mutate(rel_n_cases = label_numeric) %>%
				select(-label_numeric, -n)
		} else if(type == "relative-consequent") { #heritage precedence matrix
			edges%>%
				select(-label_numeric) %>%
				group_by(consequent) %>%
				mutate(rel_consequent = n/sum(n)) %>%
				ungroup()
		} else if(type == "relative-antecedent") { #heritage precedence matrix
			edges%>%
				select(-label_numeric) %>%
				group_by(antecedent) %>%
				mutate(rel_antecedent = n/sum(n)) %>%
				ungroup()
		} else if(type == "relative") {
			edges %>%
				mutate(rel_n = n/sum(n)) %>%
				select(-label_numeric)
		} else if(type == "absolute-case") {
			edges %>%
				mutate(n_cases = label_numeric) %>%
				select(-label_numeric, -n)
		}
	}
	return(value)
}

