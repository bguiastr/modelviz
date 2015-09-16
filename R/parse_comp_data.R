parse_comp_data <- function(mod_file   = NULL,
                            parsed_ext = NULL,
                            advan      = NULL,
                            trans      = NULL,
                            verbose    = FALSE) {

  # Check inputs ------------------------------------------------------------
  if(is.null(advan) || is.null(trans)) {
    stop('Arguments \"advan\" and \"trans\" required.')
  }

  if(!advan %in% c(1:4,11:12) && (is.null(mod_file) || is.null(parsed_ext))) {
    stop('Argument \"mod_file\" and \"parsed_ext\" required when DES is used.')
  }

  if(advan == 10) {
    stop('ADVAN 10 is not currently supported by modelviz.')
  }

  if(trans > 4) {
    stop('TRANS > 4 are not currently supported by modelviz.')
  }

  # Get compartment labels --------------------------------------------------
  if(advan %in% c(1:4, 11:12)) {
    comp_labels <- switch(as.character(advan),
                          '1'  = c('Central'),
                          '2'  = c('Depot', 'Central'),
                          '3'  = c('Central', 'Periph'),
                          '4'  = c('Depot', 'Central', 'Periph'),
                          '11' = c('Central', 'Periph 1', 'Periph 2'),
                          '12' = c('Depot', 'Central', 'Periph 1', 'Periph 2'))
  } else {
    # When DES is used
    comp_labels <- mod_file$CODE[mod_file$ABREV == 'MOD']

    if(any(grepl('NCOMP\\s*=', comp_labels))) {
      comp_labels  <- as.numeric(gsub('.+=\\s*', '', comp_labels[grepl('NCOMP.*=', comp_labels)]))
      comp_labels  <- paste('Comp', 1:comp_labels)
    } else if(all(grepl('^\\s*COMP\\s*=', comp_labels))) {
      comp_labels  <- gsub('^.+\\(|,.*$|\\).*$', '', comp_labels)
      comp_labels  <- paste0(toupper(substr(comp_labels, 1, 1)),
                             tolower(substr(comp_labels, 2, nchar(comp_labels))))
    } else {
      comp_labels  <- NULL
    }
  }

  # Get compartment scaling parameters --------------------------------------
  if(advan %in% c(1:4, 11:12)) {
    prm_template <- data.frame(advan  = rep(c(1:4, 11:12), each = 2),
                               config = c(1, 2),
                               vc     = c('VC', 'V', 'VC', 'V', 'VC', 'V1', 'VC', 'V2', 'VC', 'V1', 'VC', 'V2'),
                               vp1    = c(rep(NA, 4), 'VP', 'V2', 'VP', 'V3', 'VP1', 'V2', 'VP1', 'V3'),
                               vp2    = c(rep(NA, 8), 'VP2', 'V3', 'VP2', 'V4'),
                               stringsAsFactors = FALSE)
    prm_template <- unlist(prm_template[prm_template$advan == advan &
                                          prm_template$vc == intersect(colnames(parsed_ext$theta), prm_template$vc),
                                        c('vc', 'vp1', 'vp2')])

    if(length(prm_template) == 0) {
      msg(paste0('Warning: failed to create \"comp_prm\" for ADVAN ', advan, '.'), verbose)
    }

    comp_prm <- rep(NA, length(comp_labels))
    comp_prm[comp_labels != 'Depot'] <- prm_template[1:length(comp_labels[comp_labels != 'Depot'])]

  } else {
    # When DES is used
    comp_prm <- NA
  }

  # Create parsed_comp ------------------------------------------------------
  parsed_comp <- data.frame(label  = comp_labels,
                            prm    = comp_prm,
                            output = FALSE,
                            stringsAsFactors = FALSE)


  # Add output compartments -------------------------------------------------
  if(advan %in% c(1:4, 11:12)) {
    parsed_comp$output[which(comp_labels == 'Central')] <- TRUE
  } else {
    # When DES is used
    parsed_comp$output <- parsed_comp$output
  }

  return(parsed_comp)

} # End parse_comp_data
