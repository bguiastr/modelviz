#' Create compartment layout
#'
#' @description Combine information for each compartment's rank, label, size,
#' color, scaling, etc.
#'
#' @param prms a numerical vector of parameter values
#' @param rse a numerical vector of parameter uncertainty
#' @param advan the value of the nonmem subroutine ADVAN
#' @param scaling logical if \code{TRUE} compartment size and colors will be scaled.
#'  If \code{FALSE} standard model diagram will be created
#' @param scale.fun function to be used for compartment size scaling
#' @param box.ratio value for the compartment aspect ratio
#' @param filled logical if \code{TRUE} compartment will be filled.
#'  If \code{FALSE} only the compartment edges will be drawn
#' @param font font name of the compartment labels
#' @param comp.fontsize font size of the compartment labels in point size

#' @details The default \code{scaling.fun} argument is set to be the cubic root, each
#'  compartment will be sized to the cubic root of the volume value. This scaling
#'  allows for a more realistic size comparison and avoids too big differences in size.

#' @seealso \code{\link{import_qmd_info}}, \code{\link{qmd}}
#' @return A \code{data.frame}
#' @export
define_comp_layout <- function(prms = NA,
                               rse = NA,
                               advan = NULL,
                               des_info = NULL,
                               scaling = TRUE,
                               scale.fun = function(x) { x^(1/3) },
                               box.ratio = 3/4,
                               filled = TRUE,
                               font = 'Avenir',
                               comp.fontsize=12,...) {

  check_prms <- function(x, check, uncert = FALSE, ...) {
    if(length(setdiff(check, names(x))) > 0) {
      message(paste('make_comp_layout: Missing',
                    paste(setdiff(check, names(x)), collapse=', '),
                    ifelse(uncert, 'RSE', 'value'), 'in ADVAN', advan))
    }
    x[setdiff(check, names(x))] <- NA
    return(x)
  }

  if(advan == 1){
    prms <- check_prms(prms, 'V', FALSE)
    rse  <- check_prms(rse, 'V', TRUE)
    node <- DiagrammeR::create_nodes(nodes    = paste0('A', 1:2),
                                     label    = c('Central', 'Output'),
                                     rank     = c(1, 1),
                                     prm      = c(prms['V'], NA, recursive=TRUE),
                                     rse      = c(rse['V'], NA, recursive=TRUE))
  }

  if(advan == 2) {
    prms <- check_prms(prms, 'V', FALSE)
    rse  <- check_prms(rse, 'V', TRUE)
    if(any(is.na(rse))) {
      warning("Assuming RSE of 10%")
      rse  <- list (V = .1)
    }
    node <- DiagrammeR::create_nodes(nodes    = paste0('A',1:3),
                                     label    = c('Depot','Central','Output'),
                                     rank     = c(1,2,2),
                                     prm      = c(NA,prms['V'],NA,recursive=TRUE),
                                     rse      = c(NA,rse['V'],NA,recursive=TRUE))
  }


  if(advan == 3){
    prms <- check_prms(prms,c('V1','V2'),F)
    rse  <- check_prms(rse,c('V1','V2'),T)
    node <- DiagrammeR::create_nodes(nodes    = paste0('A',1:3),
                                     label    = c('Central','Peripheral','Output'),
                                     rank     = c(1,2,1),
                                     prm      = c(prms['V1'],prms['V2'],NA,recursive=TRUE),
                                     rse      = c(rse['V1'],rse['V2'],NA,recursive=TRUE))
  }

  if(advan == 4){
    prms <- check_prms(prms,c('V2','V3'),F)
    rse  <- check_prms(rse,c('V2','V3'),T)
    node <- DiagrammeR::create_nodes(nodes    = paste0('A',1:4),
                                     label    = c('Depot','Central','Peripheral','Output'),
                                     rank     = c(1,2,3,2),
                                     prm      = c(NA,prms['V2'],prms['V3'],NA,recursive=TRUE),
                                     rse      = c(NA,rse['V2'],rse['V3'],NA,recursive=TRUE))
  }

  if(advan == 11){
    prms <- check_prms(prms,c('V1','V2','V3'),F)
    rse  <- check_prms(rse,c('V1','V2','V3'),T)
    node <- DiagrammeR::create_nodes(nodes    = paste0('A',1:4),
                                     label    = c('Central','Peripheral 1','Peripheral 2','Output'),
                                     rank     = c(1,2,2,1),
                                     prm      = c(prms['V1'],prms['V2'],prms['V3'],NA,recursive=TRUE),
                                     rse      = c(rse['V1'],rse['V2'],rse['V3'],NA,recursive=TRUE))
  }

  if(advan == 12){
    prms <- check_prms(prms,c('V2','V3','V4'),F)
    rse  <- check_prms(rse,c('V2','V3','V4'),T)
    node <- DiagrammeR::create_nodes(nodes    = paste0('A',1:5),
                                     label    = c('Depot','Central','Peripheral 1','Peripheral 2','Output'),
                                     rank     = c(1,2,3,3,2),
                                     prm      = c(NA,prms['V2'],prms['V3'],prms['V4'],NA,recursive=TRUE),
                                     rse      = c(NA,rse['V2'],rse['V3'],rse['V4'],NA,recursive=TRUE))
  }

  if(advan == 20){
    node <- DiagrammeR::create_nodes(nodes    = paste0('A',1:3),
                                     label    = c('Non-RSP','RSP','Dropout'),
                                     rank     = c(1,1,2),
                                     prm      = NA,
                                     rse      = NA,
                                     shape    = 'circle',
                                     style    = 'dashed')
  }

  if(advan %in% c(6,9,13)) {

    ## code here to handle $DES

  }

  # Add formatting to nodes -------------------------------------------------

  if(!'shape'%in%colnames(node)){
    node$shape <- 'box'
  }

  node$scale[!is.na(node$prm)] <- prm_scale(node$prm, FUN=scale.fun)

  if(all(is.na(node$rse)) | scaling==FALSE){
    node$color <- ifelse(!is.na(node$prm), 'steelblue4', 'grey70')
  } else {
    node$color[is.na(node$rse)]  <- 'grey70'
    node$color[node$rse<=0.25]   <- 'chartreuse3'
    node$color[node$rse>0.25 & node$rse<0.5] <- 'orange3'
    node$color[node$rse>0.5]     <- 'red'
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
  node$penwidth <- ifelse(node$style%in%c('filled','invisible'), 0, 1)

  if(scaling) {
    node$width  <- ifelse(!is.na(node$scale),node$scale, 1)
  }else{
    node$width  <- 1
  }

  node$height   <- node$width * ifelse(node$shape=='circle', 1, box.ratio)
  node$fontsize <- comp.fontsize * node$width
  node$fontname <- font

  node$alpha_color <- 80

  return(node)
}
