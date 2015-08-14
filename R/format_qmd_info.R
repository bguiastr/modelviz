#' Parameter translator
#'
#' @description Translate clearances into rate constant or rate constant into
#' clearances for the built-in nonmem model library (i.e. ADVAN 1-4 and 11-12)
#'
#' @param qmd_info a \code{qmd_info} object generated with \code{import_qmd_info}
#' or \code{skeleton_qmd_info}
#' @seealso \code{\link{import_qmd_info}}, \code{\link{skeleton_qmd_info}}, \code{\link{qmd}}
#' @return A \code{data.frame} \code{qmd_info} object
#' @examples
#' \dontrun{
#' qmd_info  <- import_qmd_info(dir = '../models/pk/', runno = '001')
#' qmd_info_formatted <- format_qmd_info(qmd_info)
#' }
#' @export
format_qmd_info <- function(qmd_info = NULL, verbose = FALSE) {

  if(is.null(qmd_info)) {
    stop('Argument \"qmd_info\" required.')
  }

  if(!qmd_info$advan %in% c(1:4, 11:12)) {
    msg('This function is only intended to work with built-in nonmem model
            library (i.e. ADVAN 1-4 and 11-12).', verbose)
    return(qmd_info)
  }

  msg('This function is not yet supported.', verbose)
  return(qmd_info)

} # End format_qmd_info


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
