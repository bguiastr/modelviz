#' Create and render QMD
#'
#' @description Create and renders Quantitative Model Diagrams (QMD)
#' for NONMEM models
#'
#' @param data a \code{list} containing the parameters, their RSE and the
#'  nonmem subroutine ADVAN value
#' @param horizontal logical if \code{TRUE} the layout will be horizontal
#' @param ... other arguments passed to \code{\link{make_comp}}
#'  and \code{\link{make_arrow}} see details below for
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
#' @seealso \code{\link{prm_import}}, \code{\link{prm_format}}
#' @return A graphic object
#' @examples
#' \dontrun{
#' modelviz(parameters, horizontal = FALSE)
#' }
#' @export
modelviz <- function(data, horizontal=TRUE,...){

  if(missing(data)){stop('The "data" argument is required for this function to work')}

  comp_data  <- make_comp(data$prm, data$rse, data$advan,...)

  arrow_data <- make_arrows(data$prm, data$rse, data$advan,...)

  graph      <- make_graph(nodes_df    = comp_data,
                           edges_df    = arrow_data,
                           graph_attrs = c('minlen = 2','splines = true','layout = dot',
                                           ifelse(horizontal,'rankdir = LR','rankdir = TB')))
  DiagrammeR::render_graph(graph)
}
