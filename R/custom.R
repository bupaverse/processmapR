#' @title Custom map profile
#' @description Function to create a custom map profile based on some event log attribute.
#' @details If used for edges, it will show the attribute values which related to the out-going node of the edge.#'
#' @param FUN A summary function to be called on the provided event attribute, e.g. mean, median, min, max. na.rm = T by default.
#' @param attribute The name of the case attribute to visualize (should be numeric)
#' @param units Character to be placed after values (e.g. EUR for monitary euro values)
#' @param color_scale Name of color scale to be used for nodes. Defaults to PuBu. See `Rcolorbrewer::brewer.pal.info()` for all options.
#' @param color_edges The color used for edges. Defaults to dodgerblue4.
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




custom <- function(FUN = mean, attribute, units = "", color_scale = "PuBu", color_edges = "dodgerblue4") {
  attr(FUN, "attribute") <- attribute
  attr(FUN, "units") <- units
  attr(FUN, "perspective") <- "custom"
  attr(FUN, "color") <- color_scale
  attr(FUN, "color_edges") <- color_edges

  attr(FUN, "create_nodes") <- function(precedence, type, extra_data) {

  	attribute <- sym(attr(type, "attribute"))

  	precedence %>%
  		group_by(ACTIVITY_CLASSIFIER_, from_id) %>%
  		summarize(label = type(!!attribute, na.rm = T)) %>%
  		na.omit() %>%
  		ungroup() %>%
  		mutate(color_level = label,
  			   shape = if_end(ACTIVITY_CLASSIFIER_,"circle","rectangle"),
  			   fontcolor = if_end(ACTIVITY_CLASSIFIER_, if_start(ACTIVITY_CLASSIFIER_, "chartreuse4","brown4"),  ifelse(label <= (min(label) + (5/8)*diff(range(label))), "black","white")),
  			   color = if_end(ACTIVITY_CLASSIFIER_, if_start(ACTIVITY_CLASSIFIER_, "chartreuse4","brown4"),"grey"),
  			   tooltip =  paste0(ACTIVITY_CLASSIFIER_, "\n", round(label, 2), " ",attr(type, "units")),
  			   label = paste0(ACTIVITY_CLASSIFIER_, "\n", round(label, 2), " ",attr(type, "units")),
  			   label = if_end(ACTIVITY_CLASSIFIER_, ACTIVITY_CLASSIFIER_, tooltip))
  }

  attr(FUN, "create_edges") <- 	function(precedence, type, extra_data) {

  	attribute <- sym(attr(type, "attribute"))




  	precedence %>%
  		ungroup() %>%
  		group_by(ACTIVITY_CLASSIFIER_, next_act, from_id, to_id) %>%
  		summarize(value = type(!!attribute, na.rm = T),
  				  label = round(type(!!attribute, na.rm = T),2)) %>%
  		na.omit() %>%
  		ungroup() %>%
  		mutate(penwidth = rescale(value, to = c(1,5))) %>%
  		mutate(label = paste0(label, " ", attr(type, "units"))) %>%
  		select(-value)

  }


  return(FUN)
}
