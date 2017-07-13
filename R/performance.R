#' @title Performance map profile
#' @description Function to create a performance map profile to be used as the type of a process map. It results in a process map describing process time.
#' @param FUN A summary function to be called on the process time of a specific activity, e.g. mean, median, min, max
#' @param units The time unit in which processing time should be presented (mins, hours, days, weeks)
#' @export performance




performance <- function(FUN = mean, units = "days") {
	attr(FUN, "perspective") <- "performance"
	attr(FUN, "units") <- units
	return(FUN)
}
