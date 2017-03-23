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
hex_color <- function(color = NULL, alpha = 1) {

  if (!is.numeric(alpha) || alpha > 1 || alpha < 0) {
    stop('Argument \"alpha\" must be a number between 0 and 1')
  }

  color <- col2rgb(col = color)
  color <- rgb(red   = color[1, ],
               green = color[2, ],
               blue  = color[3, ],
               alpha = 255*alpha,
               maxColorValue = 255)

  return(color)

} # End hex_colors
