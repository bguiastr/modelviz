#' Create skeleton for \code{qmd_info}
#'
#' @description Create skeleton for \code{qmd_info} as a manual alternative to \code{import_qmd_info}
#'
#' @param help logical if \code{TRUE} help will be added in the \code{qmd_info} skeleton.
#'
#' @seealso \code{\link{import_qmd_info}}, \code{\link{read_nmtab}}, \code{\link{parse_nommem_model}}
#' @return A list containing the individuals (\code{tvprm}) and population (\code{data}) parameters,
#' parameter uncertainty (\code{rse}) the nonmem ADVAN (\code{advan}) and TRANS (\code{trans})
#' as well as the parsed differencial equation (\code{des_info}).
#' @examples
#' \dontrun{
#' qmd_info <- skeleton_qmd_info()
#' }
#' @export
skeleton_qmd_info <- function(help = TRUE) {

  if(help) {
    out <- list(tvprm    = 'Parameter typical values [named vector, required]',
                rse      = 'Parameter uncertainty in % [named vector, optional]',
                data     = 'Individual parameter values [data.frame, optional]',
                advan    = 'Value of the nonmem ADVAN [integer, required]',
                trans    = 'Value of the nonmem TRANS [integer, required with advan 1-4 or 11-12]',
                des_info = 'Parsed $DES block [vector of strings, required when advan is not 1-4 or 11-12]'
    )
  } else {
    out <- list(tvprm    = NULL,
                rse      = NULL,
                data     = NULL,
                advan    = NULL,
                trans    = NULL,
                des_info = NULL)
  }
  return(out)

} # End skeleton_qmd_info
