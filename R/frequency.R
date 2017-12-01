#' @title Frequency map profile
#' @description Function to create a frequency profile for a process map.
#' @param value The type of frequency value to be used: absolute or relative.
#' @export frequency


frequency <- function(value = c("absolute", "relative")) {
	value <- match.arg(value)
	attr(value, "perspective") <- "frequency"
	return(value)
}

