#' Create and render QMD
#'
#' @description Create and renders Quantitative Model Diagrams (QMD)
#' for NONMEM models
#'
#' @param qmd_info a \code{list} containing the parameters, their RSE and the
#'  nonmem subroutine ADVAN value
#' @param horizontal logical if \code{TRUE} the layout will be horizontal
#' @param shiny logical if \code{TRUE} output will be formated for shiny output
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
#' qmd(qmd_info, horizontal = FALSE)
#' }
#' @export

qmd <- function(qmd_info, horizontal = TRUE, shiny = FALSE,...) {

  # Check inputs
  if(is.null(qmd_info)) {
    stop('Argument \"qmd_info\" required')
  }

  # Define compartments
  comp_data  <- define_comp_layout(qmd_info, ...)

  # Define lines/arrows between compartments
  arrow_data <- define_arrow_layout(qmd_info, ...)

  # Define diagram
  graph      <- define_graph(nodes_df    = comp_data,
                             edges_df    = arrow_data,
                             graph_attrs = c('minlen = 2',
                                             'splines = true',
                                             'layout = dot',
                                             ifelse(horizontal,'rankdir = LR', 'rankdir = TB')))
  # Create plot
  if(shiny) {
    DiagrammeR::grViz(graph$dot_code)
  } else {
    DiagrammeR::render_graph(graph, ...)
  }

}
