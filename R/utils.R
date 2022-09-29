
#' @export
dplyr::`%>%`

case_id_ <- function(eventlog) sym(case_id(eventlog))
activity_id_ <- function(eventlog) sym(activity_id(eventlog))
activity_instance_id_ <- function(eventlog) sym(activity_instance_id(eventlog))
resource_id_ <- function(eventlog) sym(resource_id(eventlog))
timestamp_ <- function(eventlog) sym(timestamp(eventlog))
lifecycle_id_ <- function(eventlog) sym(lifecycle_id(eventlog))

deprecated_y_arg <- function(s, ...) {
	l <- list(...)
	if(length(l) > 0 && any(stringr::str_detect("y",names(l)))) {
		warning("Arguments y is deprecated. Use sort instead.")
		l[stringr::str_detect("y",names(l))][[1]]
	} else {
		s
	}
}

summary_statistics <- function(vector) {


	s <- summary(vector)
	s <- c(s, St.Dev = sd(vector))
	s <- c(s, IQR = s[5] - s[2])
	names(s) <- c("min","q1","median","mean","q3","max","st_dev","iqr")
	return(s)
}

if_end <- function(node, true, false) {
	ifelse(node %in% c("ARTIFICIAL_START","ARTIFICIAL_END"), true, false)
}
if_start <- function(node, true, false) {
	ifelse(node %in% c("ARTIFICIAL_START"), true, false)
}

return_plotly <- function(p, plotly) {
	if (plotly) {
		return(ggplotly(p))
	} else {
		return(p)
	}
}

# Warning: The `eventlog` argument of `func()` is deprecated as of processmapR 0.4.0.
# Please use the `log` argument instead.
# WARNING: Works only on exported functions!
lifecycle_warning_eventlog <- function (log, eventlog = deprecated()) {

	cl <- sys.call(-1L)
	func <- get(as.character(cl[[1L]]), mode = "function", envir = sys.frame(-2L))
	func_name <- match.call(definition = func, call = cl)[[1L]]

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn("0.4.0", paste0(func_name, "(eventlog)"), paste0(func_name, "(log)"))
		return(eventlog)
	}

	return(log)
}

# Warning: The `.abbreviate` argument of `func()` is deprecated as of processmapR 0.5.2.
# Please use the `abbreviate` argument instead.
# WARNING: Works only on exported functions!
lifecycle_warning_abbreviate <- function (abbreviate, .abbreviate = deprecated()) {

	cl <- sys.call(-1L)
	func <- get(as.character(cl[[1L]]), mode = "function", envir = sys.frame(-2L))
	func_name <- match.call(definition = func, call = cl)[[1L]]

	if(lifecycle::is_present(.abbreviate)) {
		lifecycle::deprecate_warn("0.5.2", paste0(func_name, "(.abbreviate)"), paste0(func_name, "(abbreviate)"))
		return(.abbreviate)
	}

	return(abbreviate)
}
