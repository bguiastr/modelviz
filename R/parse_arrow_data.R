parse_arrow_data <- function(des_block = NULL,
                             advan     = NULL,
                             trans     = NULL,
                             verbose   = FALSE) {

  # Check inputs ------------------------------------------------------------
  if (is.null(advan) || is.null(trans)) {
    stop('Arguments \"advan\" and \"trans\" required.')
  }

  if (!advan %in% c(1:4,11:12) && is.null(des_block)) {
    stop('Argument \"des_block\" required when DES is used.')
  }

  if (advan == 10) {
    stop('ADVAN 10 is not currently supported by modelviz.')
  }

  if (trans > 4) {
    stop('TRANS > 4 are not currently supported by modelviz.')
  }

  # Assign parameters to known advan ----------------------------------------
  if (advan == 1) {
    if (trans == 1) {
      parsed_arrow <- data.frame(from  = 1,
                                 to    = 2,
                                 prm   = 'K',
                                 dir   = 'forward',
                                 stringsAsFactors = FALSE)
    } else {
      parsed_arrow <- data.frame(from  = 1,
                                 to    = 2,
                                 prm   = 'CL',
                                 dir   = 'forward',
                                 stringsAsFactors = FALSE)
    }
  }

  if (advan == 2) {
    if (trans == 1) {
      parsed_arrow <- data.frame(from  = c(1, 2),
                                 to    = c(2, 3),
                                 prm   = c('KA', 'K'),
                                 dir   = 'forward',
                                 stringsAsFactors = FALSE)
    } else {
      parsed_arrow <- data.frame(from  = c(1, 2),
                                 to    = c(2, 3),
                                 prm   = c('KA', 'CL'),
                                 dir   = 'forward',
                                 stringsAsFactors = FALSE)
    }
  }

  if (advan == 3) {
    if (trans == 1) {
      parsed_arrow <- data.frame(from  = c(1, 1, 2),
                                 to    = c(3, 2, 1),
                                 prm   = c('K', 'K12', 'K21'),
                                 dir   = 'forward',
                                 stringsAsFactors = FALSE)
    } else {
      parsed_arrow <- data.frame(from  = c(1, 1),
                                 to    = c(3, 2),
                                 prm   = c('CL', 'Q'),
                                 dir   = c('forward', 'both'),
                                 stringsAsFactors = FALSE)
    }
  }

  if (advan == 4) {
    if (trans == 1) {
      parsed_arrow <- data.frame(from  = c(1, 2, 2, 3),
                                 to    = c(2, 4, 3, 2),
                                 prm   = c('KA', 'K', 'K23', 'K32'),
                                 dir   = 'forward',
                                 stringsAsFactors = FALSE)
    } else {
      parsed_arrow <- data.frame(from  = c(1, 2, 2),
                                 to    = c(2, 4, 3),
                                 prm   = c('KA', 'CL', 'Q'),
                                 dir   = c('forward', 'forward', 'both'),
                                 stringsAsFactors = FALSE)
    }
  }

  if (advan == 11) {
    if (trans == 1) {
      parsed_arrow <- data.frame(from  = c(1, 2, 1, 3, 1),
                                 to    = c(2, 1, 3, 1, 4),
                                 prm   = c('K12', 'K21', 'K13', 'K31', 'K'),
                                 dir   = 'forward',
                                 stringsAsFactors = FALSE)
    } else {
      parsed_arrow <- data.frame(from  = c(1, 1, 1),
                                 to    = c(2, 3, 4),
                                 prm   = c('Q2', 'Q3', 'CL'),
                                 dir   = c('both', 'both', 'forward'),
                                 stringsAsFactors = FALSE)
    }
  }

  if (advan == 12) {
    if (trans == 1) {
      parsed_arrow <- data.frame(from  = c(1, 2, 3, 2, 4, 2),
                                 to    = c(2, 3, 2, 4, 2, 5),
                                 prm   = c('KA', 'K23', 'K32', 'K24', 'K42', 'K'),
                                 dir   = 'forward',
                                 stringsAsFactors = FALSE)
    } else {
      parsed_arrow <- data.frame(from  = c(1, 2, 2, 2),
                                 to    = c(2, 3, 4, 5),
                                 prm   = c('KA', 'Q3', 'Q4', 'CL'),
                                 dir   = c('forward', 'both', 'both', 'forward'),
                                 stringsAsFactors = FALSE)
    }
  }

  if (!advan %in% c(1:4, 11:12)) {
    parsed_arrow <- do.call('rbind', lapply(X = des_block, FUN = des_parser, verbose = verbose))
    parsed_arrow <- parsed_arrow[, c('from', 'to', 'prm')]
    parsed_arrow <- parsed_arrow[order(parsed_arrow$prm, parsed_arrow$to),]
    parsed_arrow <- parsed_arrow[!(duplicated(parsed_arrow$prm) & is.na(parsed_arrow$to)), ]
    parsed_arrow$from[!is.na(parsed_arrow$from)] <- parsed_arrow$from[!is.na(parsed_arrow$from)]
    parsed_arrow$to[!is.na(parsed_arrow$to)]     <- parsed_arrow$to[!is.na(parsed_arrow$to)]

    # Handle double arrows
    parsed_arrow$dir <- 'forward'
    double_arrow <- parsed_arrow[parsed_arrow$prm %in% unique(parsed_arrow$prm)[table(parsed_arrow$prm) == 2],]
    parsed_arrow <- parsed_arrow[!parsed_arrow$prm %in% unique(parsed_arrow$prm)[table(parsed_arrow$prm) == 2],]
    double_arrow <- by(double_arrow,
                       double_arrow$prm,
                       FUN = function(x) {
                         if (x$from[1] == x$to[2]) {
                           x$dir <- 'both'
                           return(x[1,])
                         }
                       })
    parsed_arrow      <- rbind(parsed_arrow,
                               do.call('rbind', double_arrow))
    parsed_arrow$from <- as.numeric(parsed_arrow$from)
    parsed_arrow$to   <- as.numeric(parsed_arrow$to)
  }

  return(parsed_arrow)

} # End parse_arrow_data
