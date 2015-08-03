#' Create and render QMD
#'
#' @description Create and renders Quantitative Model Diagrams (QMD)
#' for NONMEM models
#'
#' @param data a \code{list} containing the parameters, their RSE and the
#'  nonmem subroutine ADVAN value
#' @param horizontal logical if \code{TRUE} the layout will be horizontal
#' @param ... other arguments passed to \code{\link{define_comp_layout}}
#'  and \code{\link{define_arrow_layout}} see details below for
#'  a list of the available options

#' @details
#' \itemize{
#'  \item{scaling}
#'  \item{scale.fun}
#'  \item{font}
#'  \item{comp.fontsize}
#'  \item{arrow.fontsize}
#'  \item{box.ratio}
#'  \item{filled}
#'  \item{clearance}
#' }
#'
#' @seealso \code{\link{import_qmd_info}}, \code{\link{format_qmd_info}}
#' @return A graphic object
#' @examples
#' \dontrun{
#' qmd(parameters, horizontal = FALSE)
#' }
#' @export
qmd <- function(data, horizontal = TRUE,...) {

  if(missing(data)) {
    stop('The "data" argument is required for this function to work')
  }

  # define compartments
  comp_data  <- define_comp_layout (data$prm, data$rse, data$advan, data$des_info, ...)

  # define lines/arrows between compartments
  arrow_data <- define_arrow_layout (data$prm, data$rse, data$advan, data$des_info, ...)

  # define diagram
  graph      <- define_graph (nodes_df    = comp_data,
                              edges_df    = arrow_data,
                              graph_attrs = c('minlen = 2',
                                              'splines = true',
                                              'layout = dot',
                                              ifelse(horizontal,'rankdir = LR','rankdir = TB')))
  # create plot
  DiagrammeR::render_graph(graph)
}
