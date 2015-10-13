# Differencial equation parser for arrows and comp
des_parser <- function(des, ...) {

  if(is.null(des)) {
    stop('Argument \"des\" required.')
  }

  # Remove all spaces
  des <- gsub('\\s*', '', des)

  # If des eq
  if(!grepl('^DADT\\(\\d+\\)\\s*=\\s*', des)) {
    msg('Non DADT(.)= equation provided in $DES block.', verbose)
  } else {
    comp_num <- gsub('^DADT\\((\\d+)\\).*', '\\1', des)
    des      <- gsub('^DADT\\(\\d+\\)=', '', des)

    ## Add + sign if missing for first term
    if(!grepl('^(\\+|-)', des)) { des <- paste0('+', des) }

    ## Prepare slicing
    slices <- NULL ; nested <- 0
    tmp_string  <- unlist(strsplit(des, ''))
    for(i in seq_along(tmp_string)) {
      if(tmp_string[i] == '(') { nested = nested + 1 }
      if(tmp_string[i] == ')') { nested = nested - 1 }
      if(tmp_string[i] %in% c('+','-') & nested == 0) {
        slices <- c(slices, i)
      }
    }

    ## Slice eq
    parsed_eq <- data.frame(node = comp_num,
                            des  = tapply(tmp_string,
                                          findInterval(seq_along(tmp_string), slices),
                                          paste, collapse = ''),
                            from = NA, to = NA, prm = NA,
                            stringsAsFactors = FALSE)

    ## Sort chunks
    parsed_eq$from[grepl('^\\+', parsed_eq$des)] <- parsed_eq$des[grepl('^\\+', parsed_eq$des)]
    parsed_eq$from[grepl('^-', parsed_eq$des)]   <- comp_num

    parsed_eq$to <- ifelse(grepl('^\\+', parsed_eq$des), comp_num, NA)
    parsed_eq    <- parsed_eq[!is.na(parsed_eq$from) | !is.na(parsed_eq$to), ]

    ## Generate decimal anchor for arrows
    parsed_eq$from <- gsub('.*A\\((\\d+)\\).*', '\\1', parsed_eq$from)
    parsed_eq$to   <- gsub('.*A\\((\\d+)\\).*', '\\1', parsed_eq$to)

    ## Remove Amount to focus on parameter label
    parsed_eq$des  <- gsub('A\\(\\d+\\)|\\+|-|\\*', '', parsed_eq$des)

    ## Label PRM*A(.) type rates
    parsed_eq$prm[grepl('^\\w+$', parsed_eq$des)] <- parsed_eq$des[grepl('^\\w+$', parsed_eq$des)]

    ## Label (CL/V)*A(.) type rates
    parsed_eq$prm[grepl('^\\(\\w+\\/\\w+\\)$', parsed_eq$des)] <-
      gsub('^\\((\\w+)\\/\\w+\\)$','\\1', parsed_eq$des[grepl('^\\(\\w+\\/\\w+\\)$', parsed_eq$des)])

    # Label uncompatible rates as.is
    parsed_eq$prm[is.na(parsed_eq$prm)] <- parsed_eq$des[is.na(parsed_eq$prm)]

    return(parsed_eq)
  }
}
