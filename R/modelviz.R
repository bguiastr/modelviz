#' Create and render QMD
#'
#' @description Create and renders Quantitative Model Diagrams (QMD)
#'
#' @param data a \code{list} containing the parameters, their RSE and the
#'  nonmem $SUB ADVAN
#' @param horizontal logical if \code{TRUE} the layout will be horizontal
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
