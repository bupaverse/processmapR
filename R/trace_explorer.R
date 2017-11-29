

#' @title Trace explorer
#' @description Explore traces, ordered by relative trace frequency
#' @param eventlog Eventlog object
#' @param type Frequent or infrequenct traces to explore
#' @param coverage The percentage coverage of the trace to explore. Default is 20\% most (in)frequent
#'
#' @export trace_explorer
#'
trace_explorer <- function(eventlog, type = c("frequent","infrequent"), coverage = 0.2) {
	stop_eventlog(eventlog)
	type <- match.arg(type)



	eventlog %>% cases_light %>%
		rename_("case_classifier" = case_id(eventlog)) -> cases

	eventlog %>% traces_light %>%
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
		summarize(ts = min(timestamp_classifier)) %>%
		inner_join(cases, by = "case_classifier") %>%
		group_by(trace_id) %>%
		filter(case_classifier == first(case_classifier)) %>%
		inner_join(traces, by = "trace") %>%
		arrange(ts, event_classifier) %>%
		mutate(rank_event = seq_len(n()))  -> temp

	if(type == "frequent")
		temp <- temp %>% filter(cum_freq_lag < coverage)
	else
		temp <- temp %>% filter(cum_freq_lag > (1-coverage))

	if(nrow(temp) == 0) {
		stop("No traces selected. Consider increasing the coverage")
	}


	temp %>%
		ggplot(aes(rank_event, as.factor(trace_id))) +
		geom_tile(aes(fill = event_classifier), color = "white") +
		geom_text(aes(label = abbreviate(event_classifier)), color = "white",fontface = "bold") +
		facet_grid(reorder(paste0(round(100*relative_frequency,2),"%"), -relative_frequency)~.,scales = "free", space = "free") +
		scale_y_discrete(breaks = NULL) +
		labs(y = "Traces", x = "Activities") +
		scale_fill_discrete(name = "Activities")  +
		theme_light() +
		theme(strip.text.y = element_text(angle = 0))

}
