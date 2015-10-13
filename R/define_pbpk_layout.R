#' Make proper PBPK layout
#'
#' @description Make special layout for PBPK models with the veinous compartment on
#' the left, the arterial compartment on the right and the organs in between.
#'
#' @param comp a data.frame created by \code{define_comp_layout}
#' @param arrow a data.frame created by \code{define_arrow_layout}
#' @param pbpk_color logical if \code{TRUE} oxygenated blood is represented in red
#' and deoxygenated blood is represented in blue. If \code{FALSE} previously defined
#' color code in \code{define_comp_layout} and \code{define_arrow_layout} will be used
#' @param vein_comp_label label of the veinous compartment
#' @param artery_comp_label label of the arterial compartment
#'
#' @seealso \code{\link{define_comp_layout}}, \code{\link{define_arrow_layout}}, \code{\link{qmd}}
#' @return A \code{list}
#' @examples
#' \dontrun{
#' qmd_info <- import_qmd_info(dir = '../models/pk/', runno = '001')
#' comp     <- define_comp_layout(qmd_info)
#' arrow    <- define_arrow_layout(qmd_info)
#' pbpk     <- define_pbpk_layout(comp = comp, arrow = arrow)
#' }
#' @export
define_pbpk_layout <- function(comp              = NULL,
                               arrow             = NULL,
                               pbpk_color        = TRUE,
                               vein_comp_label   = 'venous',
                               artery_comp_label = 'arterial') {

  # Check inputs ------------------------------------------------------------
  if(is.null(comp) | is.null(arrow)) {
    stop('Arguments \"comp\" and \"arrow\" required.')
  }

  vein_comp   <- comp$node[grepl(toupper(vein_comp_label), toupper(comp$label))]
  artery_comp <- comp$node[grepl(toupper(artery_comp_label), toupper(comp$label))]

  if(length(vein_comp) == 0) {
    stop(paste('Vein compartment:', vein_comp_label, 'could not be found.'))
  } else if(length(artery_comp) == 0) {
    stop(paste('Artery compartment:', artery_comp_label, 'could not be found.'))
  }

  # Reasign rank
  comp$rank[comp$node ==  vein_comp]   <- 1
  comp$rank[comp$node ==  artery_comp] <- 9999
  comp$rank[!comp$rank %in% c(1,9999)] <- 2

  # Set pbpk color mode
  if(pbpk_color == TRUE) {
    if('fillcolor' %in% colnames(comp)) {
      comp[comp$node == artery_comp,'fillcolor'] <- '#FF8080'      # red
      comp[comp$node == vein_comp,'fillcolor']   <- 'deepskyblue3' # blue
    } else {
      comp[comp$node == artery_comp,c('color', 'fontcolor')] <- '#FF8080'      # red
      comp[comp$node == vein_comp,c('color', 'fontcolor')]   <- 'deepskyblue3' # blue
    }

    arrow[arrow$from == artery_comp | arrow$to == artery_comp,
          c('color', 'fontcolor')]     <- '#FF8080'      # red
    arrow[arrow$to == vein_comp | arrow$from == vein_comp,
          c('color', 'fontcolor')]     <- 'deepskyblue3' # blue
  }

  # Handle intermediary comp
  out_comp  <- comp$node[comp$style == 'invisible']
  move_comp <- arrow[!arrow$from %in% c(vein_comp, artery_comp) &
                       !arrow$to %in% c(vein_comp, artery_comp, out_comp), c('from', 'to')]
  comp$rank[comp$node %in% move_comp$from] <- comp$rank[comp$node %in% move_comp$to] + 1
  ## Add: if parent move the out_comp will also move

  # Add invisible nodes and arrows to force layout
  comp_edit <- DiagrammeR::combine_nodes(comp,
                                         data.frame(node  = paste0('I', 1:length(unique(comp$rank))),
                                                    rank  = sort(unique(comp$rank)),
                                                    style = 'invisible'))

  arrow_edit <- DiagrammeR::combine_edges(arrow,
                                          data.frame(from  = paste0('I', 1:(length(unique(comp$rank))-1)),
                                                     to    = paste0('I', 2:length(unique(comp$rank))),
                                                     style = 'invis'))

  pbpk <- list(comp  = comp_edit, arrow = arrow_edit)

  return(pbpk)
} # End define_pbpk_layout
