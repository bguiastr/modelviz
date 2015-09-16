parse_ext_file <- function(ext_file = NULL,
                           mod_file = NULL,
                           verbose = TRUE,
                           interactive = TRUE) {

  # Check inputs
  if(is.null(ext_file) | is.null(mod_file)) {
    stop('Both arguments \"ext_file\" and \"mod_file\" required.')
  }

  if(!file.exists(ext_file)) {
    stop('Parameter file \".ext\" not found.')
  }

  ext_file <- read_nmtab(file = ext_file, nonmem_tab = FALSE)

  ext_file <- ext_file[, grepl('ITERATION|THETA', colnames(ext_file)) |
                         # Remove off diagonal elements
                         colnames(ext_file) %in% paste0('OMEGA(', 1:999, ',', 1:999, ')') |
                         colnames(ext_file) %in% paste0('SIGMA(', 1:999, ',', 1:999, ')') ]

  tvprm    <- ext_file[ext_file$ITERATION == -1000000000, -1]

  if(-1000000001 %in% ext_file$ITERATION) {
    rse     <- ext_file[ext_file$ITERATION == -1000000001, -1]
    rse     <- abs(100 * rse / tvprm)
    rse[tvprm == 0] <- 0
  } else {
    msg('Warning: parameter\'s standard error not available.', verbose)
    rse    <- NA
  }

  # Assign labels to TVPRM and RSE
  n_theta     <- length(grep('THETA', colnames(tvprm)))
  n_omega     <- length(grep('OMEGA', colnames(tvprm)))
  n_sigma     <- length(grep('SIGMA', colnames(tvprm)))

  theta_names <- mod_file$COMMENT[mod_file$ABREV == 'THETA']
  omega_names <- mod_file$COMMENT[mod_file$ABREV == 'OMEGA' & !is.na(mod_file$COMMENT)]
  sigma_names <- mod_file$COMMENT[mod_file$ABREV == 'SIGMA' & !is.na(mod_file$COMMENT)]

  tvprm[2, ]  <- rse
  row.names(tvprm) <- c('tvprm','rse')

  theta_prm   <- tvprm[, grep('THETA', colnames(tvprm)), drop = FALSE]
  omega_prm   <- tvprm[, grep('OMEGA', colnames(tvprm)), drop = FALSE]
  sigma_prm   <- tvprm[, grep('SIGMA', colnames(tvprm)), drop = FALSE]

  # Scale IIV to % scale
  omega_prm['tvprm',] <- sqrt(omega_prm['tvprm',])*100

  # Assign names to parameters
  if(n_theta != length(theta_names)) {
    if(interactive) {
      while(n_theta != length(theta_names)) {
        theta_names <- readline(prompt = paste('Enter the', n_theta, 'names for the thetas in',
                                               basename(file_full),' separated by commas: '))
        theta_names <- unlist(strsplit(x = theta_names, split = '\\s*,\\s*'))
      }
    } else {
      stop('$THETA labels did not match the number of thetas in the \".ext\" file.')
    }
  }

  colnames(theta_prm) <- toupper(theta_names)

  if(n_omega == length(omega_names)) {
    colnames(omega_prm) <- toupper(omega_names)
  } else {
    msg('Warning: names could not be attributed to omegas.', verbose)
  }

  if(n_sigma == length(sigma_names)) {
    colnames(sigma_prm) <- toupper(sigma_names)
  } else {
    msg('Warning: names could not be attributed to sigmas.', FALSE) # Not used for now
  }

  # Match omega_names with theta_names
  colnames(omega_prm) <- gsub(paste0('.*(', paste0(colnames(theta_prm), collapse = '|'),
                                     ').*'), '\\1', colnames(omega_prm))

  return(list(theta = theta_prm, omega = omega_prm, sigma = sigma_prm))

} # End parse_ext_file
