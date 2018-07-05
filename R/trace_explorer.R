

#' @title Trace explorer
#' @description Explore traces, ordered by relative trace frequency
#' @param eventlog Eventlog object
#' @param type Frequent or infrequenct traces to explore
#' @param coverage The percentage coverage of the trace to explore. Default is 20\% most (in)frequent
#' @param raw_data Retrun raw data
#' @param .abbreviate If TRUE, abbreviate activity labels
#' @param scale_fill Set color scale
#'
#'
#' @export trace_explorer
#'
trace_explorer <- function(eventlog,
						   coverage = 0.2,
						   type = c("frequent","infrequent"),
						   .abbreviate = T,
						   show_labels = T,
						   scale_fill = scale_fill_discrete(h = c(0,360) + 15, l = 40),
						   raw_data = F) {
	stopifnot("eventlog" %in% class(eventlog))
	type <- match.arg(type)

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

	eventlog %>% case_list %>%
		rename_("case_classifier" = case_id(eventlog)) -> cases

	eventlog %>% trace_list %>%
		mutate(rank_trace = row_number(-absolute_frequency)) %>%
		arrange(-relative_frequency) %>%
		mutate(cum_freq = cumsum(relative_frequency)) %>%
		mutate(cum_freq_lag = lag(cum_freq, default = 0)) -> traces


	eventlog %>%
		rename_("case_classifier" = case_id(eventlog),
				"aid" = activity_instance_id(eventlog),
				"event_classifier" = activity_id(eventlog),
				"timestamp_classifier" = timestamp(eventlog)) %>%
		as.data.frame %>%
		group_by(case_classifier, event_classifier, aid) %>%
		summarize(ts = min(timestamp_classifier),
				  min_order = min(.order)) %>%
		inner_join(cases, by = "case_classifier") %>%
		group_by(trace_id) %>%
		filter(case_classifier == first(case_classifier)) %>%
		inner_join(traces, by = "trace") %>%
		arrange(ts, min_order) %>%
		mutate(rank_event = seq_len(n()))  -> temp

	if(type == "frequent")
		temp <- temp %>% filter(cum_freq_lag < coverage)
	else
		temp <- temp %>% filter(cum_freq_lag > (1-coverage))

	if(nrow(temp) == 0) {
		stop("No traces selected. Consider increasing the coverage")
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
			facet_grid(reorder(paste0(round(100*relative_frequency,2),"%"), -relative_frequency)~.,scales = "free", space = "free") +
			scale_y_discrete(breaks = NULL) +
			labs(y = "Traces", x = "Activities") +
			scale_fill  +
			labs(fill = "Activity") +
			theme_light() +
			theme(strip.text.y = element_text(angle = 0)) -> p

		if(show_labels)
			p + geom_text(aes(label = ABBR(.abbreviate)(event_classifier)), color = "white",fontface = "bold")
		else
			p

	}

}
