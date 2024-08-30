
#' Render process map
#'
#' @inheritParams DiagrammeR::render_graph
#' @param map A `process_map` created with [`process_map`][`processmapR::process_map`] and argument `render = F`.
#'
#' @export
#'
render_map <- function(map,
					   layout = NULL,
					   output = NULL,
					   as_svg = FALSE,
					   title = NULL,
					   width = NULL,
					   height = NULL) {


	start_time <- end_time <- next_start_time <- next_end_time <- act <- from_id <- label <- NULL

	DiagrammeR::render_graph(graph = map,
							 output = output,
							 as_svg = as_svg,
							 title = title,
							 width = width,
							 height = height) -> graph

	graph$x$diagram %>%
		stringr::str_replace_all(", len = ", ", weight = ") %>%
		stringr::str_replace_all(", decorate = ", ", constraint = ") -> diagram


	rendered_process <- diagram

	sizes <- data.frame()
	colors <- data.frame()
	images <- data.frame()
	opacities <- data.frame()
	tokens <- data.frame()
	timeline_start <- 0
	timeline_end <- 0
	timeline <- FALSE
	a_factor <- 0

	precedence <- attr(map, "base_precedence") %>%
		mutate_at(vars(start_time, end_time, next_start_time, next_end_time), as.numeric, units = "secs")
	activities <- precedence %>%
		select(act, id = from_id) %>%
		stats::na.omit() %>%
		distinct() %>%
		arrange(id)

	start_activity <- map$nodes_df %>% filter(label == "Start") %>% pull(id)
	end_activity <- map$nodes_df %>% filter(label == "End") %>% pull(id)

	token_callback_onclick = c("function(svg_root, svg_element, case_id) {","}")
	token_callback_select = token_select_decoration()
	activity_callback_onclick = c("function(svg_root, svg_element, activity_id) {","}")
	activity_callback_select = activity_select_decoration()


	x <- list(
		rendered_process = rendered_process,
		activities = activities,
		tokens = tokens,
		sizes = sizes,
		sizes_scale = list(attribute = NULL, scale = "identity", domain = NULL, range = NULL),
		colors = colors,
		colors_scale = list(attribute = NULL, scale = "identity", domain = NULL, range = NULL),
		opacities = opacities,
		opacities_scale = list(attribute = NULL, scale = "identity", domain = NULL, range = NULL),
		images = images,
		images_scale = list(attribute = NULL, scale = "identity", domain = NULL, range = NULL),
		shape = "circle", #TODO see if this can be a scale too
		attributes =list(),
		start_activity = start_activity,
		end_activity = end_activity,
		# duration = 60,
		timeline = F,
		mode = "off",
		# initial_state = "playing",
		# initial_time = 0,
		# repeat_count = 1,
		# repeat_delay = 0.5,
		# jitter = 0,
		# factor = a_factor * 1000,
		# legend = NULL,
		# timeline_start = timeline_start * 1000,
		# timeline_end = timeline_end * 1000,
		# onclick_token_callback = htmlwidgets::JS(token_callback_onclick),
		# onclick_token_select = htmlwidgets::JS(token_callback_select),
		# onclick_activity_callback = htmlwidgets::JS(activity_callback_onclick),
		# onclick_activity_select = htmlwidgets::JS(activity_callback_select),
		processmap_renderer = "graph"
	)
	x <- c(x,list(
		svg_fit = T,
		svg_contain = F,
		svg_resize_fit = T,
		zoom_controls = T,
		zoom_initial = NULL
	))

	htmlwidgets::createWidget(elementId = NULL,
							  name = "processmapR",
							  x = x,
							  width = NULL, height = NULL,
							  sizingPolicy = htmlwidgets::sizingPolicy(
							  	browser.fill = TRUE,
							  	viewer.fill = TRUE,
							  	knitr.figure = FALSE,
							  	knitr.defaultWidth = "100%",
							  	knitr.defaultHeight = "300"
							  ),
							  preRenderHook = NULL,
							  dependencies = list(htmltools::htmlDependency(name = "viz.js",
							  											  version = "2.1.2",
							  											  src = c(file="htmlwidgets/lib/viz"),
							  											  script = c("viz.js",
							  											  		   "full.render.js"),
							  											  all_files = FALSE,
							  											  package = "processmapR")))
}
