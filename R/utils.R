#' @importFrom rlang sym
#'

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
	ifelse(node %in% c("Start","End"), true, false)
}
if_start <- function(node, true, false) {
	ifelse(node %in% c("Start"), true, false)
}
