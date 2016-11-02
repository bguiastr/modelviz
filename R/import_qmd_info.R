#' Import model parameters
#'
#' @description Import model parameters from NONMEM run
#'
#' @param dir location of the model files
#' @param prefix prefix of the model file name
#' @param runno run number to be evaluated
#' @param ext model file extention
#' @param file full file name as an alternative to \code{dir}, \code{prefix},
#' \code{runno} and \code{ext}
#' @param interactive if \code{TRUE} parameter labels can not be associated with their thetas, omega
#' and sigma, the user will be asked to manually provide labels. If \code{FALSE} an error will be
#' returned instead
#' @param verbose if \code{TRUE} messages are printed to the console
#'
#' @seealso \code{\link{format_qmd_info}}, \code{\link{qmd}}
#' @return A list containing the fixed effect (\code{theta}), random effect variance (\code{omega})
#' typical values along with their uncertainty, the indivudual parameters (\code{data})
#' the nonmem ADVAN (\code{advan}), the model differential equations (\code{des}),
#' the parsed compartment information (\code{parsed_comp}),
#' and the parsed arrow information (\code{parsed_arrow}).
#' @examples
#' \dontrun{
#' qmd_info <- import_qmd_info(dir = '../models/pk/', runno = '001')
#' }
#' @export
import_qmd_info <- function(dir         = NULL,
                            prefix      = 'run',
                            runno       = NULL,
                            ext         = '.mod',
                            file        = NULL,
                            interactive = TRUE,
                            verbose     = FALSE) {


  # Check inputs ------------------------------------------------------------
  if (is.null(runno) & is.null(file)) {
    stop('Argument \"runno\" or \"file\" required.')
  }

  if (!is.null(file)) {
    file_full <- file
    dir       <- paste0(dirname(file_full), '/')
  } else {

    if (!is.null(dir) && !substr(dir, nchar(dir), nchar(dir)) == '/') {
      dir <- paste0(dir, '/')
    }

    if (!ext %in% c('.ctl', '.mod', '.lst', '.txt')) {
      stop('Argument \"ext\" must be one of: \".ctl\", \".mod\", \".lst\" or \".txt\".')
    }

    file_full <- paste0(dir, prefix, runno, ext)
  }

  if (!file.exists(file_full)) {
    stop(paste('file', basename(file_full), 'not found.'))
  }


  # Import parsed model -----------------------------------------------------
  mod_file  <- parse_nonmem_model(file = file_full)


  # Get model description ---------------------------------------------------
  descr <- paste0(basename(file_full), ': ', mod_file$CODE[mod_file$ABREV == 'PRO'])


  # Grab ADVAN and TRANS ----------------------------------------------------
  subr <- as.numeric(sapply(unlist(strsplit(mod_file$CODE[mod_file$ABREV == 'SUB'], '\\s+')),
                            gsub, pattern = '\\D', replacement = ''))
  if (!subr[1] %in% c(1:4, 11:12)) { subr[2] <- 1 } # Set TRANS to 1 when DES


  # Import parsed patab -----------------------------------------------------
  tab_file  <- parse_patab(mod_file, dir, verbose)


  # Import parsed .ext file -------------------------------------------------
  ext_file   <- paste0(substr(x = file_full, start = 1, stop = nchar(file_full) - 3), 'ext')
  parsed_ext <- parse_ext_file(ext_file    = ext_file,
                               mod_file    = mod_file,
                               verbose     = verbose,
                               interactive = interactive)


  # Parse differential equations --------------------------------------------
  des_block <- mod_file$CODE[mod_file$ABREV == 'DES'] # Place holder


  # Parse arrow -------------------------------------------------------------
  parsed_arrow <- parse_arrow_data(des_block = des_block,
                                   advan     = subr[1],
                                   trans     = subr[2],
                                   verbose   = verbose)



  # Parse comp --------------------------------------------------------------
  parsed_comp <- parse_comp_data(mod_file   = mod_file,
                                 parsed_ext = parsed_ext,
                                 advan      = subr[1],
                                 trans      = subr[2],
                                 verbose    = verbose)

  ## Add output compartments
  if (subr[1] %in% c(1:4, 11:12)) {
    parsed_comp$output[parsed_comp$label == 'Central'] <- TRUE
  } else {
    # When DES is used
    parsed_comp$output[as.numeric(
      gsub('\\D', '', parsed_arrow$from[is.na(parsed_arrow$to)])
    )] <- TRUE
  }


  # Create the qmd_info object ----------------------------------------------
  out <- list(descr          = descr,             # Model description
              theta          = parsed_ext$theta,  # Theta and RSE (%)
              omega          = parsed_ext$omega,  # Omega(%) and RSE (%)
              data           = tab_file,          # Individual parameter values
              advan          = subr[1],           # NONMEM ADVAN
              des            = des_block,         # Differential equations
              parsed_comp    = parsed_comp,       # Parsed compartment info
              parsed_arrow   = parsed_arrow       # Parsed arrow info
  )
  return(out)

}
