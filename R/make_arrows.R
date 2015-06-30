#' Create arrow data
#'
#' @description Creates information on arrows label, witdh, color, scaling, etc.
#'
#' @param prms a numerical vector of parameter values
#' @param rse a numerical vector of parameter uncertainty
#' @param advan the nonmem $SUB ADVAN
#' @param scaling logical if \code{TRUE} arrow width and colors will be scaled
#' @param scale.fun function to be used for arrow width scaling
#' @param clearance logical if \code{TRUE} arrows will be scaled to their available
#'       clearance value. If \code{FALSE} arrows will be scaled to their available
#'       rate constant value
#' @param font font of arrow labels
#' @param node.fontsize font size of the arrow labels
#' @seealso \code{\link{prm_import}}, \code{\link{modelviz}}
#' @return A \code{data.frame} of arrows
#' @export
make_arrows <- function(prms=NULL, rse=NULL, advan=NULL, scaling=TRUE,
                        scale.fun ='cubic', font='Avenir', clearance=TRUE,
                        edge.fontsize=12,...){

  if(advan==1){
    if(clearance){
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

  if(advan==2){
    if(clearance){
      edge <- DiagrammeR::create_edges(from  = c('A1','A2'),
                                       to    = c('A2','A3'),
                                       label = c('KA','CL'),
                                       dir   = 'forward')
    }else{
      edge <- DiagrammeR::create_edges(from  = c('A1','A2'),
                                       to    = c('A2','A3'),
                                       label = c('KA','K'),
                                       dir   = 'forward')
    }
  }

  if(advan==3){
    if(clearance){
      edge <- DiagrammeR::create_edges(from  = c('A1','A1'),
                                       to    = c('A3','A2'),
                                       label = c('CL','Q'),
                                       dir   = c('forward','both'))
    }else{
      edge <- DiagrammeR::create_edges(from  = c('A1','A1','A2'),
                                       to    = c('A3','A2','A1'),
                                       label = c('K','K12','K21'),
                                       dir   = 'forward')
    }
  }

  if(advan==4){
    if(clearance){
      edge <- DiagrammeR::create_edges(from  = c('A1','A2','A2'),
                                       to    = c('A2','A4','A3'),
                                       label = c('KA','CL','Q'),
                                       dir   = c('forward','forward','both'))
    }else{
      edge <- DiagrammeR::create_edges(from  = c('A1','A2','A2','A3'),
                                       to    = c('A2','A4','A3','A2'),
                                       label = c('KA','K','K23','K32'),
                                       dir   = 'forward')
    }
  }

  if(advan==11){
    if(clearance){
      edge <- DiagrammeR::create_edges(from  = c('A1','A1','A1'),
                                       to    = c('A2','A3','A4'),
                                       label = c('Q2','Q3','CL'),
                                       dir   = c('both','both','forward'))
    }else{
      edge <- DiagrammeR::create_edges(from  = c('A1','A2','A1','A3','A1'),
                                       to    = c('A2','A1','A3','A1','A4'),
                                       label = c('K12','K21','K13','K31','K'),
                                       dir   = 'forward')
    }
  }

  if(advan==12){
    if(clearance){
      edge <- DiagrammeR::create_edges(from  = c('A1','A2','A2','A2'),
                                       to    = c('A2','A3','A4','A5'),
                                       label = c('KA','Q2','Q3','CL'),
                                       dir   = c('forward','both','both','forward'))
    }else{
      edge <- DiagrammeR::create_edges(from  = c('A1','A2','A3','A2','A4','A2'),
                                       to    = c('A2','A3','A2','A4','A2','A5'),
                                       label = c('KA','K23','K32','K24','K42','K'),
                                       dir   = 'forward')
    }
  }

  if(advan==20){
    edge <- DiagrammeR::create_edges(from  = c('A1','A2','A1','A2','A1','A2'),
                                     to    = c('A2','A1','A1','A2','A3','A3'),
                                     label = c('K10','K01','K00','K11','K20','K21'),
                                     dir   = 'forward',
                                     headport=c('_','_','nw','ne','_','_'),
                                     tailport=c('_','_','_','_','_','_'))
  }

  # Additional formatting ---------------------------------------------------
  edge$prm <- NA
  if(!is.null(prms)){
    edge$prm[match(intersect(edge$label,names(prms)),edge$label,nomatch=0)] <- c(prms[intersect(edge$label,names(prms))],recursive=TRUE)
  }

  edge$rse <- NA
  if(!is.null(rse)){
    edge$rse[match(intersect(edge$label,names(rse)),edge$label,nomatch=0)] <- c(rse[intersect(edge$label,names(rse))],recursive=TRUE)
  }

  if(all(is.na(edge$rse)) | scaling==FALSE){
    edge$color <- ifelse(!is.na(edge$prm),'grey40','grey70')
  }else{
    edge$color[is.na(edge$prm) & is.na(edge$rse)]  <- 'grey70'
    edge$color[!is.na(edge$prm) & is.na(edge$rse)] <- 'grey40'
    edge$color[edge$rse<=0.3]   <- 'chartreuse3'
    edge$color[edge$rse>0.3 & edge$rse<0.5] <- 'orange2'
    edge$color[edge$rse>0.5]    <- 'red'
  }

  edge$fontcolor <- edge$color

  edge$scale <- NA
  if(clearance){
    edge$scale[grep('^[CL|Q]',toupper(edge$label))] <- prm_scale(edge$prm[grep('^[CL|Q]',toupper(edge$label))],scale.fun)
  }else{
    edge$scale[grep('^K',toupper(edge$label))] <- prm_scale(edge$prm[grep('^K',toupper(edge$label))],scale.fun)
  }
  edge$scale[is.na(edge$scale)] <- 1

  if(scaling){
    edge$penwidth  <- ifelse(!is.na(edge$scale),edge$scale,1)
    edge$arrowsize <- ifelse(!is.na(edge$scale),(edge$scale*0.005)^0.23,0.8)
  }else{
    edge$penwidth  <- 1
    edge$arrowsize <- 0.8
  }

  edge$fontsize  <- edge.fontsize*edge$penwidth
  edge$fontname  <- font

  return(edge)
}
