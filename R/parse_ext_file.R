parse_ext_file <- function(ext_file = NULL, mod_file = NULL, verbose = TRUE, interactive = TRUE) {

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
  } else {
    msg('Parameters standard error not available.', verbose)
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
      stop('$THETA labels did not match the number of thetas in the \".ext\" file.')
    }
  }

  colnames(tvprm)[grep('THETA', colnames(tvprm))] <- theta_names

  if(n_omega == length(omega_names)) {
    colnames(tvprm)[grep('OMEGA', colnames(tvprm))] <- omega_names
  } else {
    msg('Names could not be attributed to omegas.', verbose)
  }

  if(n_sigma == length(sigma_names)) {
    colnames(tvprm)[grep('SIGMA', colnames(tvprm))] <- sigma_names
  } else {
    msg('Names could not be attributed to sigmas.', verbose)
  }

  if(!is.null(rse)) { colnames(rse) <- colnames(tvprm) }

  return(list(tvprm = tvprm, rse = rse))

} # End parse_ext_file
