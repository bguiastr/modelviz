#' Create arrow connections between compartments
#'
#' @description Combine information for each arrow (origin, destination, label,
#' witdh, color, scaling and more) into a dataframe
#'
#' @param qmd_info a \code{qmd_info} object generated with \code{import_qmd_info} or
#' \code{skeleton_qmd_info}
#' @param scaling logical, if \code{TRUE} arrow width and colors will be scaled.
#' If \code{FALSE} standard model diagram will be created
#' @param scale.fun a function to be used for arrow width scaling
#' @param clearance logical, if \code{TRUE} arrows will be scaled to their available
#'       clearance value. If \code{FALSE} arrows will be scaled to their available
#'       rate constant value
#' @param font font name of arrow labels
#' @param arrow.fontsize font size of the arrow labels in point size

#' @details The default \code{scaling.fun} argument is set to \code{cubic}, to match
#'  each the method used on volumes. Arrow will be sized to the cubic root of the
#'  clearance or rate constant value.
#
#' @seealso \code{\link{define_comp_layout}}, \code{\link{import_qmd_info}}, \code{\link{qmd}}
#' @return A \code{data.frame}
#' @examples
#' \dontrun{
#' qmd_info <- import_qmd_info(dir = '../models/pk/', runno = '001')
#' arrows   <- define_arrow_layout(qmd_info)
#' }
#' @export
define_arrow_layout <- function(qmd_info = NULL, scaling = TRUE,
                                scale.fun = function(x) { x^(1/3) },
                                font = 'Avenir', clearance = TRUE,
                                arrow.fontsize = 12, ...) {

  # Check inputs
  if(is.null(qmd_info$advan)) {
    stop('advan level required in \"qmd_info\"')
  }

  # $DES parser placeholder
  if(!qmd_info$advan %in% c(1:4, 11:12)) {
    if(is.null(qmd_info$des_info)) {
      stop('des_info level required in \"qmd_info\" when $DES used')
    }
    # qmd_info$des_info to edge <- DiagrammeR::create_edges()
  }

  # Create edges with templates
  if(qmd_info$advan == 1) {
    if(clearance) {
      edge <- DiagrammeR::create_edges(from  = 'A1',
                                       to    = 'A2',
                                       label = 'CL',
                                       dir   = 'forward')
    }else{
      edge <- DiagrammeR::create_edges(from  = 'A1',
                                       to    = 'A2',
                                       label = 'K',
                                       dir   = 'forward')
    }
  }

  if(qmd_info$advan == 2) {
    if(clearance) {
      edge <- DiagrammeR::create_edges(from  = c('A1', 'A2'),
                                       to    = c('A2', 'A3'),
                                       label = c('KA', 'CL'),
                                       dir   = 'forward')
    } else {
      edge <- DiagrammeR::create_edges(from  = c('A1', 'A2'),
                                       to    = c('A2', 'A3'),
                                       label = c('KA', 'K'),
                                       dir   = 'forward')
    }
  }

  if(qmd_info$advan == 3) {
    if(clearance) {
      edge <- DiagrammeR::create_edges(from  = c('A1', 'A1'),
                                       to    = c('A3', 'A2'),
                                       label = c('CL', 'Q'),
                                       dir   = c('forward', 'both'))
    } else {
      edge <- DiagrammeR::create_edges(from  = c('A1', 'A1', 'A2'),
                                       to    = c('A3', 'A2', 'A1'),
                                       label = c('K', 'K12', 'K21'),
                                       dir   = 'forward')
    }
  }

  if(qmd_info$advan == 4) {
    if(clearance) {
      edge <- DiagrammeR::create_edges(from  = c('A1', 'A2', 'A2'),
                                       to    = c('A2', 'A4', 'A3'),
                                       label = c('KA', 'CL', 'Q'),
                                       dir   = c('forward', 'forward', 'both'))
    } else {
      edge <- DiagrammeR::create_edges(from  = c('A1', 'A2', 'A2', 'A3'),
                                       to    = c('A2', 'A4', 'A3', 'A2'),
                                       label = c('KA', 'K', 'K23', 'K32'),
                                       dir   = 'forward')
    }
  }

  if(qmd_info$advan %in% c(6,9,13)) { # placeholder
    edge <- DiagrammeR::create_edges(from  = c('A1','A2'),
    to    = c('A2','A3'),
    label = c('KA','KEL'),
    dir   = c('forward','forward'))
  }

  if(qmd_info$advan == 11) {
    if(clearance) {
      edge <- DiagrammeR::create_edges(from  = c('A1', 'A1', 'A1'),
                                       to    = c('A2', 'A3', 'A4'),
                                       label = c('Q2', 'Q3', 'CL'),
                                       dir   = c('both', 'both', 'forward'))
    } else {
      edge <- DiagrammeR::create_edges(from  = c('A1', 'A2', 'A1', 'A3', 'A1'),
                                       to    = c('A2', 'A1', 'A3', 'A1', 'A4'),
                                       label = c('K12', 'K21', 'K13', 'K31', 'K'),
                                       dir   = 'forward')
    }
  }

  if(qmd_info$advan == 12) {
    if(clearance) {
      edge <- DiagrammeR::create_edges(from  = c('A1', 'A2', 'A2', 'A2'),
                                       to    = c('A2', 'A3', 'A4', 'A5'),
                                       label = c('KA', 'Q2', 'Q3', 'CL'),
                                       dir   = c('forward', 'both', 'both', 'forward'))
    } else {
      edge <- DiagrammeR::create_edges(from  = c('A1', 'A2', 'A3', 'A2', 'A4', 'A2'),
                                       to    = c('A2', 'A3', 'A2', 'A4', 'A2', 'A5'),
                                       label = c('KA', 'K23', 'K32', 'K24', 'K42', 'K'),
                                       dir   = 'forward')
    }
  }

  if(qmd_info$advan == 20) {
    edge <- DiagrammeR::create_edges(from     = c('A1', 'A2', 'A1', 'A2', 'A1', 'A2'),
                                     to       = c('A2', 'A1', 'A1', 'A2', 'A3', 'A3'),
                                     label    = c('K10', 'K01', 'K00', 'K11', 'K20', 'K21'),
                                     dir      = 'forward',
                                     headport = c('_', '_', 'nw', 'ne', '_', '_'),
                                     tailport = c('_', '_', '_', '_', '_', '_'))
  }

  # Additional formatting ---------------------------------------------------
  tvprm <- qmd_info$tvprm
  rse   <- qmd_info$rse

  edge$prm <- NA
  if(!is.null(tvprm)){
    edge$prm[match(intersect(edge$label, names(tvprm)), edge$label, nomatch = 0)] <- c(tvprm[intersect(edge$label, names(tvprm))], recursive = TRUE)
  }

  edge$rse <- NA
  if(!is.null(rse)){
    edge$rse[match(intersect(edge$label, names(rse)), edge$label, nomatch = 0)] <- c(rse[intersect(edge$label, names(rse))], recursive = TRUE)
  }

  if(all(is.na(edge$rse)) | scaling == FALSE){
    edge$color <- ifelse(!is.na(edge$prm), 'grey40', 'grey70')
  }else{
    edge$color[is.na(edge$prm) & is.na(edge$rse)]  <- 'grey70'
    edge$color[!is.na(edge$prm) & is.na(edge$rse)] <- 'grey40'
    edge$color[edge$rse <= 25]                     <- 'chartreuse3'
    edge$color[edge$rse > 25 & edge$rse < 50]      <- 'orange2'
    edge$color[edge$rse > 50]                      <- 'red'
  }

  edge$fontcolor <- edge$color

  edge$scale <- NA
  if(clearance){
    edge$scale[grep('^[CL|Q]', toupper(edge$label))] <- scale.fun(edge$prm[grep('^[CL|Q]', toupper(edge$label))])
  }else{
    edge$scale[grep('^K', toupper(edge$label))] <- scale.fun(edge$prm[grep('^K', toupper(edge$label))])
  }
  edge$scale[is.na(edge$scale)] <- 1

  if(scaling){
    edge$penwidth  <- ifelse(!is.na(edge$scale), edge$scale, 1)
    edge$arrowsize <- ifelse(!is.na(edge$scale), (edge$scale*0.005)^0.23, 0.8)
  }else{
    edge$penwidth  <- 1
    edge$arrowsize <- 0.8
  }

  edge$fontsize  <- arrow.fontsize*edge$penwidth
  edge$fontname  <- font

  return(edge)
}
