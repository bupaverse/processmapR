


scale_fill_continuous_bupaR <- function(guide = "colourbar", na.value = "grey50", name = waiver()) {
	scale_fill_gradient(low = "#339999",
						high = "#00282A",
						guide = guide,
						na.value = na.value,
						name = name)
}

col_vector <- function() {
	c("#339999", "#8ADA8A", "#F9F871", "#008FAD","#6C6DAF", "#A44165", "#70D0CF", "#00C9CE", "#009EC0", "#FF8749", "#956F5D", "#B67E64", "#7F4C35","#FFE7D3",
	  "#324B4B", "#95B1B0", "#7E88BC", "#4A5787", "#007851", "#EEE8A9", "#E6F4F1", "#428E78", "#5DBE7C", "#1F884B", "#95F7B1", "#BB73AB",
	  "#849237", "#AC76C6", "#A93800", "#00282A", "#001816","#005E5E", "#BEEB7B", "#3681B7", "#935592") -> colors
	set.seed(42)
	sample(colors) %>%
		return()
}

scale_fill_discrete_bupaR <- function(guide = "legend", na.value = "grey50", name = waiver()) {
	scale_fill_manual(guide = guide, na.value = na.value, name = name, values = col_vector())
}





