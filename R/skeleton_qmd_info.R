#' Create skeleton for \code{qmd_info}
#'
#' @description Create skeleton for \code{qmd_info} as a manual alternative to \code{import_qmd_info}
#'
#' @param help logical if \code{TRUE} help will be added in the \code{qmd_info} skeleton.
#'
#' @seealso \code{\link{import_qmd_info}}, \code{\link{read_nmtab}}, \code{\link{parse_nonmem_model}}
#' @return A list containing the fixed effect (\code{theta}), random effect variance (\code{omega})
#' typical values along with their uncertainty, the indivudual parameters (\code{data})
#' the nonmem ADVAN (\code{advan}), the parsed compartment information (\code{parsed_comp}),
#' and the parsed arrow information (\code{parsed_arrow}).
#' @examples
#' \dontrun{
#' qmd_info <- skeleton_qmd_info()
#' }
#' @export
skeleton_qmd_info <- function(help = TRUE) {

  if(help) {
    out <- list(descr        = 'Model description [character string, optional]',
                theta        = 'Theta typical values and RSE (%) [data.frame, required]',
                omega        = 'Omega typical values (%) and RSE (%) [data.frame, optional]',
                data         = 'Individual parameter values [data.frame, optional]',
                advan        = 'Nonmem ADVAN subroutine [integer, required]',
                parsed_comp  = 'Parsed compartment information [data.frame, required]',
                parsed_arrow = 'Parsed arrow information [data.frame, required]'
    )
  } else {
    out <- list(descr        = NULL,
                theta        = NULL,
                omega        = NULL,
                data         = NULL,
                advan        = NULL,
                parsed_comp  = data.frame(label  = as.character(),
                                          prm    = as.character(),
                                          output = as.character()),
                parsed_arrow = data.frame(from = as.character(),
                                          to   = as.character(),
                                          prm  = as.character(),
                                          dir  = as.character())
                )
  }
  return(out)

} # End skeleton_qmd_info
