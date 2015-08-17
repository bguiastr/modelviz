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
#' @param interactive if \code{TRUE} parameter labels can not be matched with their thetas, omega
#' and sigma, the user will be asked to manually provide labels. If \code{FALSE} an error will be
#' returned instead
#' @param verbose if \code{TRUE} messages are printed to the console
#'
#' @seealso \code{\link{format_qmd_info}}, \code{\link{qmd}}
#' @return A list containing the individuals (\code{tvprm}) and population (\code{data}) parameters,
#' parameter uncertainty (\code{rse}) the nonmem ADVAN (\code{advan}) and TRANS (\code{trans})
#' as well as the parsed differencial equation (\code{des_info}).
#' @examples
#' \dontrun{
#' qmd_info <- import_qmd_info(dir = '../models/pk/', runno = '001')
#' }
#' @export
import_qmd_info <- function(dir = NULL,
                            prefix = 'run',
                            runno = NULL,
                            ext = '.mod',
                            file = NULL,
                            interactive = TRUE,
                            verbose = FALSE) {

  # Check inputs
  if(is.null(runno) & is.null(file)) {
    stop('Argument \"runno\" or \"file\" required.')
  }

  if(!is.null(file)) {
    file_full <- file
    dir       <- paste0(dirname(file_full), '/')
  } else {

    if(!is.null(dir) && !substr(dir, nchar(dir), nchar(dir)) == '/') {
      dir <- paste0(dir, '/')
    }

    if(!ext %in% paste0('.', c('ctl', 'mod', 'lst', 'txt'))) {
      stop('Argument \"ext\" must be one of: \".ctl\", \".mod\", \".lst\" or \".txt\".')
    }

    file_full <- paste0(dir, prefix, runno, ext)
  }

  if(!file.exists(file_full)) {
    stop(paste('file', basename(file_full), 'not found.'))
  }


  # Import parsed model
  mod_file  <- parse_nonmem_model(file = file_full)


  # Grab ADVAN and TRANS
  subr <- as.numeric(sapply(unlist(strsplit(mod_file$CODE[mod_file$ABREV == 'SUB'], '\\s+')),
                            gsub, pattern = '\\D', replacement = ''))
  if(!subr[1] %in% c(1:4, 11:12)) { subr[2] <- 1 } # Set TRANS to 1 when DES


  # Import parsed patab
  tab_file  <- parse_patab(mod_file, dir, verbose)


  # Import parsed .ext file
  ext_file   <- paste0(substr(x = file_full, start = 1, stop = nchar(file_full)-3), 'ext')
  parsed_ext <- parse_ext_file(ext_file, mod_file, verbose, interactive)


  # Create output object
  out <- list(tvprm    = parsed_ext$tvprm,    # Parameters typical values
              rse      = parsed_ext$rse,      # Parameters uncertainty
              data     = tab_file,            # Individual parameter values
              advan    = subr[1],             # NONMEM ADVAN
              trans    = subr[2],             # NONMEM trans
              des_info = NULL                 # $DES: just a placeholder for now
  )
  return(out)

} # End import_qmd_info
