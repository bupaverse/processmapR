#' @title Trace explorer
#' @description Explore traces, ordered by relative trace frequency
#' @param type Frequent traces first, or infrequent traces first?
#' @param coverage The percentage coverage of the trace to explore. Default is 20% most (in)frequent
#' @param n_traces Instead of setting coverage, you can set an exact number of traces. Should be an integer larger than 0.
#' @param raw_data Return raw data
#' @param .abbreviate If TRUE, abbreviate activity labels
#' @param show_labels If False, activity labels are not shown.
#' @param label_size Font size of labels
#' @param scale_fill Set color scale
#' @param coverage_labels Change the labels to be shown on the right of the process variants. These can be relative frequency (default), absolute, or cumulative.
#' @inheritParams dotted_chart
#' @importFrom stats reorder
#'
#' @export trace_explorer

trace_explorer <- function(log,
						   coverage = NULL,
						   n_traces = NULL,
						   type = c("frequent","infrequent"),
						   coverage_labels = c("relative","absolute","cumulative"),
						   .abbreviate = T,
						   show_labels = T,
						   label_size = 3,
						   scale_fill = processmapR:::scale_fill_discrete_bupaR(),
						   raw_data = F,
						   eventlog = deprecated()) {
	UseMethod("trace_explorer")
}

#' @describeIn trace_explorer Trace explorer eventlog
#' @export

trace_explorer.eventlog <- function(log,
						   coverage = NULL,
						   n_traces = NULL,
						   type = c("frequent","infrequent"),
						   coverage_labels = c("relative","absolute","cumulative"),
						   .abbreviate = T,
						   show_labels = T,
						   label_size = 3,
						   scale_fill = processmapR:::scale_fill_discrete_bupaR(),
						   raw_data = F,
							eventlog = deprecated()) {
	type <- match.arg(type)


	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn("0.4.0",
								  "trace_explorer(eventlog)",
								  "trace_explorer(log)")
		log <- eventlog
	}

	if(is.null(coverage) & is.null(n_traces)) {
		coverage <- ifelse(type == "frequent",0.2, 0.05)
		warning(glue::glue("No coverage or number of traces set. Defaulting to {coverage} for {type} traces."))
	} else if(!is.null(coverage) & !is.null(n_traces)) {
		stop("coverage and n_traces cannot be set at the same time. Use one and set the other to NULL")
	} else if(is.null(coverage)) {
		if(n_traces <= 0)
			stop("n_traces should be greater than zero.")
	}




	min_order <- NULL

	event_classifier <- NULL
	absolute_frequency <- NULL
	relative_frequency <- NULL
	cum_freq <- NULL
	case_classifier <- NULL
	aid <- NULL
	timestamp_classifier <- NULL
	trace_id <- NULL
	ts <- NULL
	cum_freq_lag <- NULL
	rank_event <- NULL

	log %>% case_list %>%
		rename("case_classifier" := !!case_id_(log)) -> cases

	# sort descending or ascending?
	sort_factor <- ifelse(type == "frequent", -1, 1)

	log %>% trace_list %>%
		mutate(rank_trace = row_number(-absolute_frequency)) %>%
		arrange(sort_factor*relative_frequency) %>%
		mutate(cum_freq = cumsum(relative_frequency)) %>%
		mutate(cum_freq_lag = lag(cum_freq, default = 0)) -> traces

	x <- nrow(traces)


	if(!is.null(coverage))
		traces <- traces %>% filter(cum_freq_lag < coverage)
	else if(type == "frequent")
		traces <- traces %>% arrange(-relative_frequency) %>% slice(1:n_traces)
	else
		traces <- traces %>% arrange(relative_frequency) %>% slice(1:n_traces)

	if(is.null(coverage)) {
		if(x < n_traces)
		warning("Fewer traces found than specified number.")
	}


	if(nrow(traces) == 0) {
		stop("No traces selected. Consider increasing the coverage")
	}


	log %>%
		rename("case_classifier" := !!case_id_(log),
				"aid" := !!activity_instance_id_(log),
				"event_classifier" := !!activity_id_(log),
				"timestamp_classifier" := !!timestamp_(log)) %>%
		as.data.frame %>%
		arrange(timestamp_classifier, .order) %>%
		# distinct keeps first entry (=minimum)
		distinct(case_classifier, event_classifier, aid, .keep_all = TRUE) %>%
		rename(ts = timestamp_classifier,
	           min_order = .order) %>%
		inner_join(cases, by = "case_classifier") %>%
		group_by(trace_id) %>%
		filter(case_classifier == first(case_classifier)) %>%
		inner_join(traces, by = "trace") %>%
		arrange(ts, min_order) %>%
		mutate(rank_event = seq_len(n())) %>%
		ungroup() %>%
		mutate(facets_rel = reorder(paste0(round(100*relative_frequency,2),"%"), -relative_frequency)) %>%
		mutate(facets_abs = reorder(absolute_frequency, -relative_frequency)) %>%
		mutate(facets_cum = reorder(paste0(round(100*cum_freq,2),"%"), cum_freq)) -> temp

	if(length(coverage_labels) > 1) {

		recode(rev(coverage_labels),
			   "relative" = "facets_rel",
			   "absolute" = "facets_abs",
			   "cumulative" = "facets_cum") -> coverage_labels




		facets <- as.formula(paste0(paste(coverage_labels, collapse = "+"), "~."))
	}
	else  {
		coverage_labels <- match.arg(coverage_labels)
		if(coverage_labels == "relative") {
			facets <- facets_rel~.
		} else if(coverage_labels == "absolute") {
			facets <- facets_abs~.
			} else if(coverage_labels == "cumulative") {
			facets <- facets_cum~.

		}
	}


	ABBR <- function(do_abbreviate) {
		if(do_abbreviate) {
			abbreviate
		} else
			function(value) {
				str_wrap(value, 20)
			}
	}


	if(raw_data)
		temp
	else {

		temp %>%
			ggplot(aes(rank_event, as.factor(trace_id))) +
			geom_tile(aes(fill = event_classifier), color = "white") +
			facet_grid(facets,scales = "free", space = "free") +
			scale_y_discrete(breaks = NULL) +
			labs(y = "Traces", x = "Activities") +
			scale_fill  +
			labs(fill = "Activity") +
			theme_light() +
			theme(strip.background = element_rect(color = "white"),
				strip.text.y = element_text(angle = 0),
				  strip.text = element_text(size = 11)) -> p

		if(show_labels)
			p + geom_text(aes(label = ABBR(.abbreviate)(event_classifier)), color = "white",fontface = "bold", size = label_size)
		else
			p

	}

}

#' @describeIn trace_explorer Trace explorer activity log
#' @export
#'
trace_explorer.activitylog <- function(log,
									coverage = NULL,
									n_traces = NULL,
									type = c("frequent","infrequent"),
									coverage_labels = c("relative","absolute","cumulative"),
									.abbreviate = T,
									show_labels = T,
									label_size = 3,
									scale_fill = processmapR:::scale_fill_discrete_bupaR(),
									raw_data = F,
									eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn("0.4.0",
								  "trace_explorer(eventlog)",
								  "trace_explorer(log)")
		log <- eventlog
	}

	log %>% bupaR::to_eventlog() %>%
		trace_explorer(coverage, n_traces, type, coverage_labels, .abbreviate, show_labels, label_size, scale_fill, raw_data)
}


#' @rdname trace_explorer
#' @export plotly_trace_explorer

plotly_trace_explorer <- function(eventlog,
								  coverage = NULL,
								  n_traces = NULL,
								  type = c("frequent","infrequent"),
								  coverage_labels = c("relative","absolute","cumulative"),
								  .abbreviate = T,
								  show_labels = T,
								  label_size = 3,
								  scale_fill = processmapR:::scale_fill_discrete_bupaR(),
								  raw_data = F) {

	trace_explorer(eventlog,
				   coverage = coverage,
				   coverage_labels = coverage_labels,
				   n_traces = n_traces,
				   type = type,
				   .abbreviate = .abbreviate,
				   show_labels = show_labels,
				   label_size = label_size,
				   scale_fill = scale_fill,
				   raw_data = raw_data) %>%
		ggplotly()
}
