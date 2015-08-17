#' Create compartment layout
#'
#' @description Combine information for each compartment's rank, label, size,
#' color, scaling, etc.
#'
#' @param qmd_info a \code{qmd_info} object generated with \code{import_qmd_info} or
#' \code{skeleton_qmd_info}
#' @param scaling logical if \code{TRUE} compartment size and colors will be scaled.
#'  If \code{FALSE} standard model diagram will be created
#' @param scale.fun a function to be used for compartment size scaling
#' @param box.ratio value for the compartment aspect ratio
#' @param filled logical if \code{TRUE} compartment will be filled.
#'  If \code{FALSE} only the compartment edges will be drawn
#' @param font font name of the compartment labels
#' @param comp.fontsize font size of the compartment labels in point size

#' @details The default \code{scaling.fun} argument is set to be the cubic root, each
#'  compartment will be sized to the cubic root of the volume value. This scaling
#'  allows for a more realistic size comparison and avoids too big differences in size.

#' @seealso \code{\link{define_arrow_layout}}, \code{\link{import_qmd_info}}, \code{\link{qmd}}
#' @return A \code{data.frame}
#' @examples
#' \dontrun{
#' qmd_info <- import_qmd_info(dir = '../models/pk/', runno = '001')
#' comp     <- define_comp_layout(qmd_info)
#' }
#' @export
define_comp_layout <- function(qmd_info = NULL,
                               scaling = TRUE,
                               scale.fun = function(x) { x^(1/3) },
                               box.ratio = 3/4,
                               filled = TRUE,
                               font = 'Avenir',
                               comp.fontsize = 12,
                               ...) {

  # Check inputs
  if(is.null(qmd_info)) {
    stop('Argument \"qmd_info\" required.')
  }

  # $DES parser placeholder
  if(!qmd_info$advan %in% c(1:4, 11:12)) {
    if(is.null(qmd_info$des_info)) {
      stop('des_info level required in \"qmd_info\" when $DES used.')
    }
    # qmd_info$des_info to node <- DiagrammeR::create_nodes()
  }

  check_prms <- function(x, check, uncert = FALSE, ...) {
    if(length(setdiff(check, names(x))) > 0) {
      message(paste('make_comp_layout: Missing',
                    paste(setdiff(check, names(x)), collapse=', '),
                    ifelse(uncert, 'RSE', 'value'), 'in ADVAN', qmd_info$advan))
    }
    x[setdiff(check, names(x))] <- NA
    return(x)
  }

  if(qmd_info$advan == 1) {
    VC   <- intersect(c('V','VC','V1','V2'),names(qmd_info$tvprm))
    prms <- check_prms(qmd_info$tvprm, VC, FALSE)
    rse  <- check_prms(qmd_info$rse, VC, TRUE)
    node <- DiagrammeR::create_nodes(nodes = paste0('A', 1:2),
                                     label = c('Central', 'Output'),
                                     rank  = c(1, 1),
                                     prm   = c(prms[VC], NA, recursive = TRUE),
                                     rse   = c(rse[VC], NA, recursive = TRUE))
  }

  if(qmd_info$advan == 2) {
    VC   <- intersect(c('V','VC','V1','V2'),names(qmd_info$tvprm))
    prms <- check_prms(qmd_info$tvprm, VC, FALSE)
    rse  <- check_prms(qmd_info$rse, VC, TRUE)
    node <- DiagrammeR::create_nodes(nodes = paste0('A', 1:3),
                                     label = c('Depot', 'Central', 'Output'),
                                     rank  = c(1, 2, 2),
                                     prm   = c(NA, prms[VC], NA, recursive = TRUE),
                                     rse   = c(NA, rse[VC], NA, recursive = TRUE))
  }

  if(qmd_info$advan == 3){
    VC   <- ifelse('VC' %in% names(qmd_info$tvprm), 'VC', 'V1')
    VP   <- ifelse('VP' %in% names(qmd_info$tvprm), 'VP', 'V2')
    prms <- check_prms(qmd_info$tvprm, c(VC, VP), FALSE)
    rse  <- check_prms(qmd_info$rse, c(VC, VP), TRUE)
    node <- DiagrammeR::create_nodes(nodes = paste0('A', 1:3),
                                     label = c('Central', 'Peripheral', 'Output'),
                                     rank  = c(1, 2, 1),
                                     prm   = c(prms[VC], prms[VP], NA, recursive = TRUE),
                                     rse   = c(rse[VC], rse[VP], NA, recursive = TRUE))
  }

  if(qmd_info$advan == 4){
    VC   <- ifelse('VC' %in% names(qmd_info$tvprm), 'VC', 'V2')
    VP   <- ifelse('VP' %in% names(qmd_info$tvprm), 'VP', 'V3')
    prms <- check_prms(qmd_info$tvprm, c(VC, VP), FALSE)
    rse  <- check_prms(qmd_info$rse, c(VC, VP), TRUE)
    node <- DiagrammeR::create_nodes(nodes = paste0('A', 1:4),
                                     label = c('Depot', 'Central', 'Peripheral', 'Output'),
                                     rank  = c(1, 2, 3, 2),
                                     prm   = c(NA, prms[VC], prms[VP], NA, recursive = TRUE),
                                     rse   = c(NA, rse[VC], rse[VP], NA, recursive = TRUE))
  }

  if(qmd_info$advan == 11){
    VC    <- ifelse('VC' %in% names(qmd_info$tvprm), 'VC', 'V1')
    VP1   <- ifelse('VP1' %in% names(qmd_info$tvprm), 'VP1', 'V2')
    VP2   <- ifelse('VP2' %in% names(qmd_info$tvprm), 'VP2', 'V3')
    prms <- check_prms(qmd_info$tvprm, c(VC, VP1, VP2), FALSE)
    rse  <- check_prms(qmd_info$rse, c(VC, VP1, VP2), TRUE)
    node <- DiagrammeR::create_nodes(nodes = paste0('A', 1:4),
                                     label = c('Central', 'Peripheral 1', 'Peripheral 2', 'Output'),
                                     rank  = c(1, 2, 2, 1),
                                     prm   = c(prms[VC], prms[VP1], prms[VP2], NA, recursive = TRUE),
                                     rse   = c(rse[VC], rse[VP1], rse[VP2], NA, recursive = TRUE))
  }

  if(qmd_info$advan %in% c(6,9,13)) {
    l <- length(des_info)
    node <- DiagrammeR::create_nodes(nodes    = paste0("A", 1:l),
                                     label    = paste0("A", 1:l),
                                     rank     = rep(1, l),
                                     prm      = rep(10, l),
                                     rse      = rep(.1, l))
  }
  if(qmd_info$advan == 12){
    VC    <- ifelse('VC' %in% names(qmd_info$tvprm), 'VC', 'V2')
    VP1   <- ifelse('VP1' %in% names(qmd_info$tvprm), 'VP1', 'V3')
    VP2   <- ifelse('VP2' %in% names(qmd_info$tvprm), 'VP2', 'V4')
    prms <- check_prms(qmd_info$tvprm, c(VC, VP1, VP2), FALSE)
    rse  <- check_prms(qmd_info$rse, c(VC, VP1, VP2), TRUE)
    node <- DiagrammeR::create_nodes(nodes = paste0('A', 1:5),
                                     label = c('Depot', 'Central', 'Peripheral 1', 'Peripheral 2', 'Output'),
                                     rank  = c(1, 2, 3, 3, 2),
                                     prm   = c(NA, prms[VC], prms[VP1], prms[VP2], NA, recursive = TRUE),
                                     rse   = c(NA, rse[VC], rse[VP1], rse[VP2], NA, recursive = TRUE))
  }

  if(qmd_info$advan == 20){
    node <- DiagrammeR::create_nodes(nodes = paste0('A', 1:3),
                                     label = c('Non-RSP', 'RSP', 'Dropout'),
                                     rank  = c(1, 1, 2),
                                     prm   = NA,
                                     rse   = NA,
                                     shape = 'circle',
                                     style = 'dashed')
  }

  # Needs to be disscussed
  #     if(any(is.na(node$rse) & !is.na(node$prm))) {
  #       warning("Assuming RSE of 10%")
  #       node$rse[is.na(node$rse) & !is.na(node$prm)]  <- 10
  #     }

  # Add formatting to nodes -------------------------------------------------

  if(!'shape' %in% colnames(node)){
    node$shape <- 'box'
  }

  node$scale[!is.na(node$prm)] <- scale.fun(node$prm[!is.na(node$prm)])

  if(all(is.na(node$rse)) | scaling == FALSE){
    node$color <- ifelse(!is.na(node$prm), 'steelblue4', 'grey70')
  } else {
    node$color[is.na(node$rse)]               <- 'grey70'
    node$color[node$rse <= 25]                <- 'chartreuse3'
    node$color[node$rse > 25 & node$rse < 50] <- 'orange3'
    node$color[node$rse > 50]                 <- 'red'
  }

  if(filled) {
    node$style <- 'filled'
  } else{
    node$style <- 'solid'
    node$fontcolor   <- node$color
  }

  node$style[grepl('^OUT', toupper(node$label))]  <- 'invisible'
  node$style[grepl('DEPOT', toupper(node$label))] <- 'dashed'
  node$shape[grepl('DEPOT', toupper(node$label))] <- 'circle'
  node$penwidth <- ifelse(node$style %in% c('filled','invisible'), 0, 1)

  if(scaling) {
    node$width  <- ifelse(!is.na(node$scale), node$scale, 1)
  }else{
    node$width  <- 1
  }

  node$height   <- node$width * ifelse(node$shape == 'circle', 1, box.ratio)
  node$fontsize <- comp.fontsize * node$width
  node$fontname <- font

  node$alpha_color <- 90#as.hexmode(1*255)

  return(node)
}
