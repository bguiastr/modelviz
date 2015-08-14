parse_patab <- function(mod_file = NULL, dir = NULL, verbose = FALSE){

  # Check inputs
  if(is.null(mod_file)) {
    stop('Argument \"mod_file\" required.')
  }

  # Extract file name
  tab_file  <- unlist(sapply(strsplit(grep(pattern = '.*FILE\\s*=\\s*patab.*',
                                           x = mod_file$CODE[mod_file$ABREV == 'TAB'],
                                           value = TRUE), '.*FILE\\s*=\\s*'), '[', 2))

  # Ensure file exsits
  tab_file  <- tab_file[file.exists(paste0(dir, tab_file))]

  if(is.null(tab_file) | length(tab_file) == 0) {
    msg(paste0('Parameter table \"patab\" not available.'), verbose)
    tab_file <- NULL
  } else {
    tab_file  <- read_nmtab(file = paste0(dir, tab_file))

    # Clean up file
    if(!'ID' %in% colnames(tab_file)) {
      msg('Missing \"ID\" column in patab.', verbose)
      tab_file <- NULL
    } else {
      tab_file <- tab_file[!duplicated(tab_file[,'ID']),
                           !duplicated(colnames(tab_file)) &
                             !colnames(tab_file) %in% c('DV', 'PRED', 'RES', 'WRES')]
    }
  }
  return(tab_file)

} # End parse_patab
