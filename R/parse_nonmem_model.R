#' NONMEM model file parser
#'
#' @description Parse NONMEM model files in R format
#'
#' @param dir location of the model files
#' @param prefix prefix of the model file name
#' @param runno run number to be evaluated
#' @param ext model file extention
#' @param file full file name as an alternative to \code{dir}, \code{prefix},
#' \code{runno} and \code{ext}
#'
#' @seealso \code{\link{import_qmd_info}}, \code{\link{read_nmtab}}
#' @return A \code{data.frame} containing the parsed model code (\code{CODE}) as well as a numeric
#' (\code{LEVEL}) and character (\code{SUB}) indexing for subroutines.
#' @examples
#' \dontrun{
#' mod_file <- parse_nonmem_model(dir = '../models/pk/', runno = '101')
#' }
#' @export
parse_nonmem_model <- function(dir = NULL, runno = NULL, prefix = 'run', ext = '.mod', file = NULL) {

  # Check inputs
  if(is.null(runno) & is.null(file)) {
    stop('Argument \"runno\" or \"file\" required.')
  }

  if(!is.null(dir) && !substr(dir, nchar(dir), nchar(dir)) == '/') {
    dir <- paste0(dir, '/')
  }

  if(!is.null(file)) {
    file_full <- file
  } else {
    file_full <- paste0(dir, prefix, runno, ext)
  }

  if(!file.exists(file_full)) { stop(paste('file', file_full, 'not found.')) }

  # Import mod_file
  mod_file <- readLines(file_full)

  # Clean-up code
  mod_file <- mod_file[!grepl('^;.+$|^$', mod_file)]
  mod_file <- gsub('\\t+|\\s{2,}', ' ', mod_file)

  # Index mod_file
  mod_file <- data.frame(CODE  = mod_file,
                         LEVEL = findInterval(seq_along(mod_file), grep('^\\s*\\$.+', mod_file)),
                         stringsAsFactors = FALSE)

  mod_file <- by(data = mod_file, INDICES = mod_file$LEVEL,
                 FUN  = function(x) {
                   x$SUB <- gsub('^(\\s*\\$\\w+)\\s.+', '\\1', x[1,'CODE'])
                   return(x)
                 })
  mod_file <- do.call('rbind', mod_file)

  # If lst file index lst rows
  if(any(grepl('NM-TRAN MESSAGES', mod_file[, 1], fixed = TRUE))) {
    lst <- grep('NM-TRAN MESSAGES', mod_file[, 1], fixed = TRUE):nrow(mod_file)
    mod_file[lst, 'LEVEL'] <- mod_file[lst, 'LEVEL'] + 1
    mod_file[c(1,lst), 'SUB']   <- '$LST'
  }

  # Further format mod_file
  mod_file$ABREV   <- substr(x = mod_file$SUB, start = 2,
                             stop = ifelse(grepl('THETA|OMEGA|SIGMA', mod_file$SUB),
                                           nchar(mod_file$SUB), 4)) # Handle priors
  mod_file$ABREV   <- gsub('\\s+', '', mod_file$ABREV)

  mod_file$CODE    <- gsub('^\\s*\\$\\w+\\s*', '', mod_file$CODE)
  mod_file         <- mod_file[!grepl('^\\s*$', mod_file$CODE), ]

  # Extract comments
  mod_file$COMMENT[grepl(';', mod_file$CODE)] <- gsub('^.+;\\s*', '\\1',
                                                      mod_file$CODE[grepl(';', mod_file$CODE)])
  mod_file$COMMENT <- gsub('\\s+$|^\\s+', '', mod_file$COMMENT)
  mod_file$CODE <- gsub('\\s*;.*', '', mod_file$CODE)

  # Sort columns
  mod_file <- mod_file[, c('LEVEL', 'SUB', 'ABREV', 'CODE', 'COMMENT')]

  return(mod_file)

} # End parse_nonmem_model
