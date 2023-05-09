#' @title Trace Explorer
#'
#' @description
#' Different activity sequences in the `log` can be visualized with [`trace_explorer()`]. With the `type` argument,
#' it can be used to explore frequent as well as infrequent traces. The `coverage` argument specificies how much of the
#' `log` you want to explore. By default it is set at `0.2`, meaning that it will show the most (in)frequency traces
#' covering 20% of the `log`.
#'
#' @param log [`log`][`bupaR::log`]: Object of class [`log`][`bupaR::log`] or derivatives ([`eventlog`][`bupaR::eventlog`]
#' or [`activitylog`][`bupaR::activitylog`]).
#' @param coverage [`numeric`] (default `0.2`): The percentage coverage of the trace to explore. Defaults to `0.2` (`0.05`) most (in)frequent.
#' @param n_traces [`integer`]: Instead of setting `coverage`, an exact number of traces can be set. Should be an [`integer`] larger than `0`.
#' @param type [`character`] (default `"frequent"`): `"frequent"` traces first, or `"infrequent"` traces first?
#' @param coverage_labels [`character`] (default `"relative"`): Change the labels to be shown on the right of the process variants.
#' These can be `"relative"` frequency (default), `"absolute"`, or `"cumulative"`. Multiple labels can be selected at the same time.
#' @param .abbreviate `r lifecycle::badge("deprecated")`; please use `abbreviate` instead.
#' @param abbreviate [`logical`] (default `TRUE`): If `TRUE`, abbreviate activity labels.
#' @param show_labels [`logical`] (default `TRUE`): If `FALSE`, activity labels are not shown.
#' @param label_size [`numeric`] (default `3`): Font size of labels.
#' @param scale_fill [`ggplot2`] scale function (default [`scale_fill_discrete_bupaR`][`bupaR::scale_fill_discrete_bupaR`]):
#' Set color scale. Defaults to [`scale_fill_discrete_bupaR`][`bupaR::scale_fill_discrete_bupaR`]. Replaced with [`scale_fill_discrete`][`ggplot2::scale_fill_discrete`] when more than 26 activities are present.
#' @param raw_data [`logical`] (default `FALSE`): Return raw data instead of graph.
#'
#' @inheritParams dotted_chart
#'
#' @examples
#' library(processmapR)
#' library(eventdataR)
#'
#' patients %>%
#'  trace_explorer(coverage = 0.8)
#'
#' @export trace_explorer
trace_explorer <- function(log,
                           coverage = NULL,
                           n_traces = NULL,
                           type = c("frequent","infrequent"),
                           coverage_labels = c("relative","absolute","cumulative"),
                           abbreviate = TRUE,
                           show_labels = TRUE,
                           label_size = 3,
                           scale_fill = bupaR::scale_fill_discrete_bupaR,
                           raw_data = FALSE,
                           plotly = FALSE,
                           eventlog = deprecated(),
                           .abbreviate = deprecated()) {
  UseMethod("trace_explorer")
}

#' @describeIn trace_explorer Trace explorer for an [`eventlog`][`bupaR::eventlog`].
#' @export
trace_explorer.eventlog <- function(log,
                                    coverage = NULL,
                                    n_traces = NULL,
                                    type = c("frequent","infrequent"),
                                    coverage_labels = c("relative","absolute","cumulative"),
                                    abbreviate = TRUE,
                                    show_labels = TRUE,
                                    label_size = 3,
                                    scale_fill = bupaR::scale_fill_discrete_bupaR,
                                    raw_data = FALSE,
                                    plotly = FALSE,
                                    eventlog = deprecated(),
                                    .abbreviate = deprecated()) {

  type <- arg_match(type)
  coverage_labels <- arg_match(coverage_labels, multiple = TRUE)

  abbreviate <- lifecycle_warning_abbreviate(abbreviate, .abbreviate)

  if(lifecycle::is_present(eventlog)) {
    lifecycle::deprecate_warn("0.4.0",
                              "trace_explorer(eventlog)",
                              "trace_explorer(log)")
    log <- eventlog
  }

  if(is.null(coverage) & is.null(n_traces)) {
    coverage <- if (type == "frequent") 0.2 else 0.05

    cli_warn(c("No {.arg coverage} or {.arg n_traces} set.",
               "!" = "Defaulting to {.arg coverage} = {.val {coverage}} for {.arg type} = {.val {type}} traces."))
  } else if(!is.null(coverage) & !is.null(n_traces)) {
    cli_abort(c("{.arg coverage} and {.arg n_traces} cannot be set at the same time.",
                "x" = "Use either {.arg coverage} or {.arg n_traces} and set the other to {.cls NULL} (default)"))
  } else if(is.null(n_traces)) {
    if (!is.numeric(coverage) || is.na(coverage) || coverage < 0 || coverage > 1)
      cli_abort(c("{.arg coverage} must be a {.cls numeric} between 0 and 1.",
                  "x" = "You supplied a {.cls {class(coverage)}}: {.val {coverage}}"))
  } else if(is.null(coverage)) {
    if(n_traces <= 0 || !is_integerish(n_traces, n = 1))
      cli_abort(c("{.arg n_traces} must be an interger-like {.cls numeric} larger than {.val {0}}.",
                  "x" = "You supplied a {.cls {class(n_traces)}}: {.val {n_traces}}"))
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
  sort_factor <- if (type == "frequent") -1 else 1

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
      cli_warn("Fewer traces ({.val {x}}) found than specified {.arg n_traces} ({.val {n_traces}}).")
  }

  if(x == 0) {
    cli_abort("No traces were selected. Consider increasing the {.arg coverage}.")
  }

  log %>%
  	as.data.frame %>%
  	rename("case_classifier" := !!case_id_(log),
           "aid" := !!activity_instance_id_(log),
           "event_classifier" := !!activity_id_(log),
           "timestamp_classifier" := !!timestamp_(log)) %>%
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
    if(coverage_labels == "relative") {
      facets <- facets_rel~.
    } else if(coverage_labels == "absolute") {
      facets <- facets_abs~.
    } else if(coverage_labels == "cumulative") {
      facets <- facets_cum~.

    }
  }

  if(raw_data)
    return(temp)
  else {

  	if(length(unique(temp$event_classifier)) > 26) {
  		scale_fill <- ggplot2::scale_fill_discrete
  	}

    temp %>%
      ggplot(aes(rank_event, as.factor(trace_id))) +
      geom_tile(aes(fill = event_classifier), color = "white") +
      facet_grid(facets,scales = "free", space = "free") +
      scale_y_discrete(breaks = NULL) +
      labs(y = "Traces", x = "Activities") +
      scale_fill(name = "Activity") +
      theme_light() +
      theme(strip.background = element_rect(color = "white"),
            strip.text.y = element_text(angle = 0),
            strip.text = element_text(size = 11)) -> p

    if(show_labels)
      p + geom_text(aes(label = abbreviate_labels(abbreviate, event_classifier)),
                    color = "white",
                    fontface = "bold",
                    size = label_size) -> p
  }

  return_plotly(p, plotly)
}

#' @describeIn trace_explorer Trace explorer for an [`activitylog`][`bupaR::activitylog`].
#' @export
trace_explorer.activitylog <- function(log,
                                       coverage = NULL,
                                       n_traces = NULL,
                                       type = c("frequent","infrequent"),
                                       coverage_labels = c("relative","absolute","cumulative"),
                                       abbreviate = TRUE,
                                       show_labels = TRUE,
                                       label_size = 3,
                                       scale_fill = bupaR::scale_fill_discrete_bupaR,
                                       raw_data = FALSE,
                                       plotly = FALSE,
                                       eventlog = deprecated(),
                                       .abbreviate = deprecated()) {

  abbreviate <- lifecycle_warning_abbreviate(abbreviate, .abbreviate)

  if(lifecycle::is_present(eventlog)) {
    lifecycle::deprecate_warn("0.4.0",
                              "trace_explorer(eventlog)",
                              "trace_explorer(log)")
    log <- eventlog
  }

  log %>% bupaR::to_eventlog() %>%
    trace_explorer(coverage = coverage,
                   n_traces = n_traces,
                   type = type,
                   coverage_labels = coverage_labels,
                   abbreviate = abbreviate,
                   show_labels = show_labels,
                   label_size = label_size,
                   scale_fill = scale_fill,
                   raw_data = raw_data,
                   plotly = plotly)
}


#' @keywords internal
#' @rdname deprecated
#' @export plotly_trace_explorer
plotly_trace_explorer <- function(log,
                                  coverage = NULL,
                                  n_traces = NULL,
                                  type = c("frequent","infrequent"),
                                  coverage_labels = c("relative","absolute","cumulative"),
                                  abbreviate = TRUE,
                                  show_labels = TRUE,
                                  label_size = 3,
                                  scale_fill = bupaR::scale_fill_discrete_bupaR,
                                  raw_data = FALSE,
                                  plotly = TRUE,
                                  eventlog = deprecated(),
                                  .abbreviate = deprecated()) {

  lifecycle::deprecate_warn(when = "0.5.2",
                            what = "plotly_trace_explorer()",
                            details = "Please use `trace_explorer(..., plotly = TRUE)` instead.")

  abbreviate <- lifecycle_warning_abbreviate(abbreviate, .abbreviate)

  if(lifecycle::is_present(eventlog)) {
    lifecycle::deprecate_warn("0.4.0",
                              "trace_explorer(eventlog)",
                              "trace_explorer(log)")
    log <- eventlog
  }

  trace_explorer(log,
                 coverage = coverage,
                 coverage_labels = coverage_labels,
                 n_traces = n_traces,
                 type = type,
                 abbreviate = abbreviate,
                 show_labels = show_labels,
                 label_size = label_size,
                 scale_fill = scale_fill,
                 raw_data = raw_data,
                 plotly = TRUE)
}


abbreviate_labels <- function(abbr, label) {
  if (abbr) {
    abbreviate(label)
  } else {
    str_wrap(label, 20)
  }
}
