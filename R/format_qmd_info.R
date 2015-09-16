#' Parameter translator
#'
#' @description Translate clearances into rate constant or rate constant into
#' clearances for the built-in nonmem model library (i.e. ADVAN 1-4 and 11-12)
#'
#' @param qmd_info a \code{qmd_info} object generated with \code{import_qmd_info}
#' or \code{skeleton_qmd_info}
#' @seealso \code{\link{import_qmd_info}}, \code{\link{skeleton_qmd_info}}, \code{\link{qmd}}
#' @return A \code{qmd_info} object
#' @examples
#' \dontrun{
#' qmd_info  <- import_qmd_info(dir = '../models/pk/', runno = '001')
#' qmd_info_formatted <- format_qmd_info(qmd_info)
#' }
#' @export
format_qmd_info <- function(qmd_info = NULL,
                            verbose = TRUE) {

  if(is.null(qmd_info)) {
    stop('Argument \"qmd_info\" required.')
  }

  if(!qmd_info$advan %in% c(1:4, 11:12)) {
    msg('Warning: this function is only intended to work with built-in nonmem model library (i.e. ADVAN 1-4 and 11-12).', verbose)
    return(qmd_info)
  }

  msg('Warning: this function is not yet supported.', verbose)
  return(qmd_info)

} # End format_qmd_info
