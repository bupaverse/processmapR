#' @title Frequency map profile
#' @description Function to create a frequency profile for a process map.
#' @param value The type of frequency value to be used:
#' absolute, relative (percentage of activity instances) or relative_case (percentage of cases the activity occurs in).
#' @export frequency


frequency <- function(value = c("absolute", "relative", "relative_case")) {
	value <- match.arg(value)
	attr(value, "perspective") <- "frequency"
	return(value)
}

