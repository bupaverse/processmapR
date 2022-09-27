
col_vector <- function() {
	c("#339999", "#8ADA8A", "#FF8749", "#6C6DAF", "#008FAD", "#A44165",
	  "#005E5E", "#5DBE7C", "#A93800", "#AC76C6", "#70D0CF", "#935592",
	  "#324B4B", "#1F884B", "#956F5D", "#7E88BC", "#00C9CE", "#4A5787",
	  "#428E78", "#007851", "#7F4C35", "#3681B7", "#009EC0", "#95B1B0",
	  "#849237", "#00282A")
}

scale_fill_discrete_bupaR <- function(guide = "legend", na.value = "grey50", name = waiver()) {
	scale_fill_manual(guide = guide, na.value = na.value, name = name, values = col_vector())
}

scale_color_discrete_bupaR <- function(guide = "legend", na.value = "grey50", name = waiver()) {
	scale_color_manual(guide = guide, na.value = na.value, name = name, values = col_vector())
}

scale_fill_continuous_bupaR <- function(guide = "colourbar", na.value = "grey50", name = waiver()) {
	scale_fill_gradient(low = "#70D0CF",
						high = "#00282A",
						guide = guide,
						na.value = na.value,
						name = name)
}

scale_color_continuous_bupaR <- function(guide = "colourbar", na.value = "grey50", name = waiver()) {
	scale_color_gradient(low = "#70D0CF",
						high = "#00282A",
						guide = guide,
						na.value = na.value,
						name = name)
}

scale_fill_gradient_bupaR <- function(guide = "colourbar", na.value = "grey50", name = waiver()) {
	scale_fill_gradient(low = "#339999",
						high = "#FF8749",
						guide = guide,
						na.value = na.value,
						name = name)
}

scale_color_gradient_bupaR <- function(guide = "colourbar", na.value = "grey50", name = waiver()) {
	scale_color_gradient(low = "#339999",
						high = "#FF8749",
						guide = guide,
						na.value = na.value,
						name = name)
}

scale_fill_gradient2_bupaR <- function(guide = "colourbar", na.value = "grey50", name = waiver(), midpoint = 0) {
	scale_fill_gradient2(low = "#339999",
						 mid = "#95B1B0",
						high = "#FF8749",
						midpoint = midpoint,
						guide = guide,
						na.value = na.value,
						name = name)
}

scale_color_gradient2_bupaR <- function(guide = "colourbar", na.value = "grey50", name = waiver(), midpoint = 0) {
	scale_color_gradient2(low = "#339999",
						 mid = "#95B1B0",
						 high = "#FF8749",
						 midpoint = midpoint,
						 guide = guide,
						 na.value = na.value,
						 name = name)
}

