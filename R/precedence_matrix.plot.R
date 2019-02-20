
#' @title Process Matrix Plot
#'
#' @description  Visualize a precendence matrix. A generic plot function for precedences matrices.
#' @param x Precedence matrix
#' @param ... Additional paramters
#' @return A ggplot object, which can be customized further, if deemed necessary.
#' @method plot process_matrix

#' @export

plot.process_matrix <- function(x, ...) {

	type <- attr(x, "matrix_type")
	perspective <- attr(type, "perspective")

	flow_time <- NULL
	antecedent <- NULL
	consequent <- NULL
	rel_n <- NULL
	rel_antecedent <- NULL
	rel_consequent <- NULL
	rel_n_cases <- NULL
	if(perspective == "frequency") {
		if(type == "absolute") {

			x %>%
				ggplot(aes(antecedent, consequent)) +
				geom_tile(aes(fill = n)) +
				geom_text(aes(label = n), color = "white", fontface = "bold")  +
				scale_fill_continuous_tableau(name = "Absolute Frequency", palette = "Blue") +
				coord_flip() +
				theme_light() +
				theme(axis.text.x = element_text(angle = 45, hjust = 1),
					  legend.position = "top") -> p
			p <- p + labs(x = "Antecedent", y = "Consequent")


			return(p)
		} else if(type == "relative") {
			x %>%
				ggplot(aes(antecedent, consequent)) +
				geom_tile(aes(fill = rel_n)) +
				geom_text(aes(label = round(rel_n*100, 2)), color = "white", fontface = "bold") +
				scale_fill_continuous_tableau(name = "Relative Frequency", palette = "Blue") +
				theme_light() +
				coord_flip() +
				theme(axis.text.x = element_text(angle = 45, hjust = 1),
					  legend.position = "top")-> p
			p <- p + labs(x = "Antecedent", y = "Consequent")


			return(p)
		} else if(type == "relative-case") {
			x %>%
				ggplot(aes(antecedent, consequent)) +
				geom_tile(aes(fill = rel_n_cases)) +
				geom_text(aes(label = round(rel_n_cases*100, 2)), color = "white", fontface = "bold") +
				scale_fill_continuous_tableau(name = "Relative Case Frequency", palette = "Blue") +
				theme_light() +
				coord_flip() +
				theme(axis.text.x = element_text(angle = 45, hjust = 1),
					  legend.position = "top")-> p
			p <- p + labs(x = "Antecedent", y = "Consequent")

			return(p)
		} else if(type == "absolute-case") {
			x %>%
				ggplot(aes(antecedent, consequent)) +
				geom_tile(aes(fill = n_cases)) +
				geom_text(aes(label = n_cases), color = "white", fontface = "bold") +
				scale_fill_continuous_tableau(name = "Absolute Case Frequency", palette = "Blue") +
				theme_light() +
				coord_flip() +
				theme(axis.text.x = element_text(angle = 45, hjust = 1),
					  legend.position = "top")-> p
			p <- p + labs(x = "Antecedent", y = "Consequent")

			return(p)
		} else if(type == "relative-antecedent") {
			x %>%
				ggplot(aes(antecedent, consequent)) +
				geom_tile(aes(fill = rel_antecedent)) +
				geom_text(aes(label = round(rel_antecedent*100, 2)), color = "white", fontface = "bold") +
				facet_grid(antecedent~., scales = "free", space = "free") +
				scale_fill_continuous_tableau(name = "Relative Frequency (antecedent based)", palette = "Blue") +
				theme_light() +
				coord_flip() +
				theme(axis.text.x = element_text(angle = 45, hjust = 1),
					  strip.background = element_blank(),
					  strip.text = element_blank(),
					  legend.position = "top")-> p
			p <- p + labs(x = "Antecedent", y = "Consequent")
			return(p)
		} else if(type == "relative-consequent") {
			x %>%
				ggplot(aes(antecedent, consequent)) +
				geom_tile(aes(fill = rel_consequent)) +
				geom_text(aes(label = round(rel_consequent*100,2)), color = "white", fontface = "bold") +
				facet_grid(~consequent, scales = "free", space = "free") +
				scale_fill_continuous_tableau(name = "Relative Frequency (consequent based)", palette = "Blue") +
				theme_light() +
				coord_flip() +
				theme(axis.text.x = element_text(angle = 45, hjust = 1),
					  strip.background = element_blank(),
					  strip.text = element_blank(),
					  legend.position = "top")-> p
			p <- p + labs(x = "Antecedent", y = "Consequent")

			return(p)
		}
	} else if(perspective == "performance") {
		x %>%
			ggplot(aes(antecedent, consequent)) +
			geom_tile(aes(fill = flow_time)) +
			geom_text(aes(label =round(flow_time, 2)), color = "white", fontface = "bold") +
			scale_fill_continuous_tableau(name = paste0("Flow time in ", attr(type, "units")), palette = "Red") +
			theme_light() +
			coord_flip() +
			theme(axis.text.x = element_text(angle = 45, hjust = 1),
				  strip.background = element_blank(),
				  strip.text = element_blank(),
				  legend.position = "top")-> p
		p <- p + labs(x = "Antecedent", y = "Consequent")

		return(p)
		}
}
