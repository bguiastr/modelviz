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
#' returned instead.
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
import_qmd_info <- function(dir = NULL, prefix = 'run', runno, ext = '.mod',
                            file = NULL, interactive = TRUE) {

  # Check inputs
  if(!is.null(file)) {
    file_full <- file
    dir       <- dirname(file_full)
  } else {
    file_full <- paste0(dir, prefix, runno, ext)
  }

  if(!file.exists(file_full)) {
    stop(paste('file', file_full, 'could not be found.'))
  }


  # Import parsed model
  mod_file  <- parse_nonmem_model(file = file_full)


  # Grab ADVAN and TRANS
  subr <- as.numeric(sapply(unlist(strsplit(mod_file$CODE[mod_file$ABREV == 'SUB'], '\\s+')),
                            gsub, pattern = '\\D', replacement = ''))

  if(!subr[1] %in% c(1:4, 11:12)) {
    subr[2] <- 1  # Set TRANS to 1 when not used
  }


  # Import PRM ind
  tab_file  <- unlist(sapply(strsplit(grep(pattern = '.*FILE\\s*=\\s*patab.*',
                                    x = mod_file$CODE[mod_file$ABREV == 'TAB'],
                                    value = TRUE), '.*FILE\\s*=\\s*'), '[', 2))
  tab_file  <- tab_file[file.exists(paste0(dir, tab_file))]

  if(is.null(tab_file)) {
    stop(paste('Could not find any \"patab\" associated with',
               basename(file_full), 'under', dir))
  }

  tab_file  <- read_nmtab(file = paste0(dir, tab_file))

  if(!'ID' %in% colnames(tab_file)) {
    message('\"ID\" column required in patab: setting patab to NULL')
    tab_file <- NULL
  }  else {
    tab_file <- tab_file[!duplicated(tab_file[,'ID']),
                         !duplicated(colnames(tab_file)) &
                           !colnames(tab_file) %in% c('DV', 'PRED', 'RES', 'WRES')]
  }


  # Import TVPRM and RSE
  ext_file <- paste0(substr(x = file_full, start = 1, stop = nchar(file_full)-3), 'ext')

  if(!file.exists(ext_file)) {
    stop(paste('Could not find the \".ext\" file associated with',
               basename(ext_file), 'under', dir))
  }

  ext_file <- read_nmtab(file = ext_file)
  ext_file <- ext_file[, grepl('ITERATION|THETA', colnames(ext_file)) |
                         # Remove off diagonal elements
                         colnames(ext_file) %in% paste0('OMEGA(', 1:999, ',', 1:999, ')') |
                         colnames(ext_file) %in% paste0('SIGMA(', 1:999, ',', 1:999, ')') ]

  tvprm    <- ext_file[ext_file$ITERATION == -1000000000, -1]

  if(-1000000001 %in% ext_file$ITERATION) {
    rse    <- ext_file[ext_file$ITERATION == -1000000001, -1]
    rse    <- abs(100*rse/tvprm)
  } else {
    rse    <- NULL
  }


  # Assign labels to TVPRM and RSE
  n_theta     <- length(grep('THETA', colnames(tvprm)))
  n_omega     <- length(grep('OMEGA', colnames(tvprm)))
  n_sigma     <- length(grep('SIGMA', colnames(tvprm)))

  theta_names <- mod_file$COMMENT[mod_file$ABREV == 'THETA']
  omega_names <- mod_file$COMMENT[mod_file$ABREV == 'OMEGA']
  sigma_names <- mod_file$COMMENT[mod_file$ABREV == 'SIGMA']

  if(n_theta != length(theta_names)) {
    if(interactive) {
      while(n_theta != length(theta_names)) {
        theta_names <- readline(prompt = paste('Enter the', n_theta, 'names for the thetas in',
                                               basename(file_full),' separated by commas: '))
        theta_names <- unlist(strsplit(x = theta_names, split = '\\s*,\\s*'))
      }
    } else {
      stop('$THETA labels did not match the number of thetas in the \".ext\" file')
    }
  }

  colnames(tvprm)[grep('THETA', colnames(tvprm))] <- theta_names

  if(n_omega == length(omega_names)) {
    colnames(tvprm)[grep('OMEGA', colnames(tvprm))] <- omega_names
  } else {
    message('Names could not be attributed to omegas')
  }

  if(n_sigma == length(sigma_names)) {
    colnames(tvprm)[grep('SIGMA', colnames(tvprm))] <- sigma_names
  } else {
    message('Names could not be attributed to sigmas')
  }
  colnames(rse) <- colnames(tvprm)


  # $DES: just a placeholder for now
  des_block <- mod_file[mod_file$SUB == 'DES', 'CODE']
  des_info  <- NULL

  if(is.null(des_block)) {
    des_info <- parse_des_block(des_block)
  }


  # Create output object
  out <- list(tvprm    = tvprm,    # Parameters typical values
              rse      = rse,      # Parameters uncertainty
              data     = tab_file, # Individual parameter values
              advan    = subr[1],  # NONMEM ADVAN
              trans    = subr[2],  # NONMEM trans
              des_info = des_info  # $DES: just a placeholder for now
  )
  return(out)

} # End import_qmd_info
