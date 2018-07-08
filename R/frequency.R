#' @title Frequency map profile
#' @description Function to create a frequency profile for a process map.
#' @param value The type of frequency value to be used:
#' absolute, relative (percentage of activity instances) or relative_case (percentage of cases the activity occurs in).
#' @param color_scale Name of color scale to be used for nodes. Defaults to PuBu. See `Rcolorbrewer::brewer.pal.info()` for all options.
#' @param color_edges The color used for edges. Defaults to dodgerblue4.
#' @export frequency


frequency <- function(value = c("absolute", "relative", "absolute_case", "relative_case"), color_scale = "PuBu", color_edges = "dodgerblue4") {
	value <- match.arg(value)
	attr(value, "perspective") <- "frequency"
	attr(value, "color") <- color_scale
	attr(value, "color_edges") <- color_edges
	return(value)
}

