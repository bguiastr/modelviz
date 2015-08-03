#' Format parameter vector
#'
#' @description Translate clearances (CL, Q) and volumes (V) into
#'              rate constant and the other way around.
#'
#' @param data a \code{data.frame} of parameters
#' @param advan the nonmem $SUB ADVAN
#' @param trans the nonmem $SUB TRANS
#' @seealso \code{\link{import_qmd_info}}, \code{\link{qmd}}
#' @return A \code{data.frame} of parameters
#' @examples
#' \dontrun{
#' prm_list   <- import_qmd_info(dir='../models/pk/', runno='001')
#' parameters <- format_qmd_info(data=prm_list$prm, advan=prm_list$advan, trans=prm_list$trans)
#' }
#' @export
format_qmd_info <- function(data, advan, trans) {

  message('This function is currently not ready for use')
  return(data)

#   if(!trans%in%c(1,3,4)){stop('wrong TRANS provided for ADVAN4')}
#
#
#   # Normalize parameter names across ADVAN 3-4 and all TRANS ----------------
#
#   if(advan==3){
#     colnames(data)[colnames(data)=='V2'] <- 'V3'
#     colnames(data)[colnames(data)=='V' | colnames(data)=='V1'] <- 'V2'
#   }
#
#   if(advan==4){
#     colnames(data)[colnames(data)=='V'] <- 'V2'
#   }
#
#   colnames(data)[colnames(data)=='K'] <- 'KE'
#   c.names <- colnames(data)[colnames(data)!='ID']
#
#
#   # Imput missing parameters ------------------------------------------------
#
#   if(trans==1){
#     if(!'CL'%in%c.names){
#       data[,'CL'] <- data[,'V2']*data[,'KE']
#     }
#
#     if(!'Q'%in%c.names){
#       data[,'Q']  <- data[,'V2']*data[,'K23']
#     }
#
#     if(!'V3'%in%c.names){
#       data[,'V3'] <- data[,'Q']/data[,'K32']
#     }
#   } # End trans 1
#
#   if(trans==3){
#     if(!'V3'%in%c.names){
#       data[,'V3'] <- data[,'VSS']-data[,'V2']
#     }
#   } # End trans 3
#
#   if(trans==3 | trans==4){
#     if(!'KE'%in%c.names){
#       data[,'KE']  <- data[,'CL']/data[,'V2']
#     }
#
#     if(!'K23'%in%c.names){
#       data[,'K23']  <- data[,'Q']/data[,'V2']
#     }
#
#     if(!'K32'%in%c.names){
#       data[,'K32'] <- data[,'Q']/data[,'V3']
#     }
#   } # End trans 4
}
