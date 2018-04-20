#' @title Custom map profile
#' @description Function to create a custom map profile based on some event log attribute.
#' @details If used for edges, it will show the attribute values which related to the out-going node of the edge.#'
#' @param FUN A summary function to be called on the provided event attribute, e.g. mean, median, min, max. na.rm = T by default.
#' @param attribute The name of the case attribute to visualize (should be numeric)
#' @param units Character to be placed after values (e.g. EUR for monitary euro values)
#' @param color_scale Name of color scale to be used for nodes. Defaults to PuBu. See `Rcolorbrewer::brewer.pal.info()` for all options.
#' @examples
#' \dontrun{
#' library(eventdataR)
#' library(processmapR)
#' data(traffic_fines)
#' # make sure the amount attribute is propagated forward in each trace
#' # using zoo::na.locf instead of tidyr::fill since it is much faster
#' # still the whole pre-processing is still very slow
#' library(zoo)
#'
#' traffic_fines_prepared <- traffic_fines %>%
#' filter_trace_frequency(percentage = 0.8) %>%
#' group_by_case() %>%
#' mutate(amount = na.locf(amount, na.rm = F)) %>%
#' ungroup_eventlog()
#'
#' process_map(traffic_fines_prepared, type_nodes = custom(attribute = "amount", units = "EUR"))
#' }
#'
#' @export custom




custom <- function(FUN = mean, attribute, units = "", color_scale = "PuBu") {
  attr(FUN, "attribute") <- attribute
  attr(FUN, "units") <- units
  attr(FUN, "perspective") <- "custom"
  attr(FUN, "color") <- color_scale
  return(FUN)
}
