#' @title Export a graph to various image formats
#' @param graph A graph object of class `dgr_graph`.
#' @param file_name The name of the exported file (including it's extension).
#' @param file_type The type of file to be exported. Options for graph files
#'   are: `png`, `pdf`, `svg`, and `ps`.
#' @param title An optional title for the output graph.
#' @param width Output width in pixels or `NULL` for default. Only useful for
#'   export to image file formats `png`, `pdf`, `svg`, and `ps`.
#' @param height Output height in pixels or `NULL` for default. Only useful for
#'   export to image file formats `png`, `pdf`, `svg`, and `ps`.
#' @name export_graph
#' @importFrom DiagrammeR export_graph
#' @export
DiagrammeR::export_graph
