#' Create color in hexadecimal
#'
#' @description Create hexadecimal type color name with transparency
#'
#' @param color a hexadecimal or named color e.g. '#1874CD' or 'dodgerblue3'
#' @param alpha transparency factor. Numeric value between 0 and 1
#'
#' @examples
#' \dontrun{
#' color <- hex_color(color = 'dodgerblue3', alpha = 0.5)
#' }
#' @export
hex_color <- function(color = NULL, alpha = NULL) {

  if(is.null(color)) {
    stop('Missing the \"color\" argument.')
  }

  for(i in seq_along(color)) {
    if(!grepl('#', color[i], fixed = TRUE)) {
      if(!color[i] %in% x11_hex()[ , 1]) {
        stop('Invalid \"color\" argument.')
      } else {
        color[i] <- x11_hex()[which(color[i]==x11_hex()[ , 1]), 2]
      }
    }
  }

  alpha <- suppressWarnings(as.numeric(alpha))

  if(length(alpha) != 0 && (is.na(alpha) | alpha > 1 | alpha < 0)) {
    stop('Invalid \"alpha\" argument. Must be within 0 - 1.')
  }

  if(!is.null(alpha)) {
    alpha <- as.hexmode(round(as.numeric(alpha)*255, digits = 0))
    alpha <- gsub(' ', 0, format(alpha, width = 2), fixed = TRUE)


    color <- paste0(substr(color, 1, 7), alpha)
  }
  return(color)

} # End hex_colors
