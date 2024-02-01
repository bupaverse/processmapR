token_select_decoration <- function(stroke = "black") {

	return (paste0('function(node, selected) {
          if (selected) {
            node.attr("stroke-width", "3")
                .attr("stroke", "',stroke,'");
          } else {
            node.attr("stroke-width", "1")
                .attr("stroke", "black");
          }
        }'))

}

activity_select_decoration <- function(stroke_dasharray = "2", stroke_width = "2", stroke = "black") {

	return (paste0('function(node, selected) {
            if (selected) {
              node.attr("stroke-width", "',stroke_width,'")
                  .attr("stroke-dasharray", "',stroke_dasharray,'")
                  .attr("stroke", "',stroke,'");
            } else {
              node.attr("stroke-width", "1")
                  .attr("stroke-dasharray", "0")
                  .attr("stroke", "#c0c0c0");
            }
          }'))

}
