#' Create and render QMD
#'
#' @description Create and renders Quantitative Model Diagrams (QMD)
#' for NONMEM models
#'
#' @param qmd_info a \code{list} containing the parameters, their RSE and the
#'  nonmem subroutine ADVAN value
#' @param flipped logical if \code{TRUE} the layout will be flipped
#' @param rank integer vertor assigning a rank for each compartment. Can be used
#' to obtain a specific layout
#' @param pbpk_layout logical if \code{TRUE} a PBPK layout will be applied
#' @param shiny logical if \code{TRUE} output will be formated for shiny output
#' @param output format of the output to be returned by qmd ('graph', 'SVG', 'DOT' or 'vivagraph')
#' @param width width of the resulting graphic in pixels
#' @param height height of the resulting graphic in pixels
#' @param ... other arguments passed to \code{\link{define_comp_layout}},
#'  \code{\link{define_arrow_layout}} and \code{\link{define_pbpk_layout}} see details below for
#'  a list of the available options

#' @details
#' \itemize{
#'  \item{scaling}
#'  \item{comp_scale_fun}
#'  \item{arrow_scale_fun}
#'  \item{color_scaling}
#'  \item{color_cutoff}
#'  \item{pbpk_color}
#'  \item{filled}
#'  \item{labels}
#'  \item{alpha}
#'  \item{font}
#'  \item{comp_fontsize}
#'  \item{arrow_fontsize}
#'  \item{clearance_mode}
#'  \item{vein_comp_label}
#'  \item{artery_comp_label}
#' }
#'
#' @seealso \code{\link{import_qmd_info}}, \code{\link{format_qmd_info}}
#' @return A graphic object
#' @examples
#' \dontrun{
#' qmd(qmd_info, flipped = FALSE)
#' }
#' @export
qmd <- function(qmd_info      = NULL,
                flipped       = FALSE,
                rank          = NULL,
                pbpk_layout   = FALSE,
                color_scaling = NULL,
                shiny         = FALSE,
                output        = 'graph',
                width         = NULL,
                height        = NULL,
                ...) {

  # Check inputs ------------------------------------------------------------
  if(is.null(qmd_info)) {
    stop('Argument \"qmd_info\" required.')
  }

  if(is.null(color_scaling) && pbpk_layout) {
    color_scaling <- 'pbpk'
  } else if(is.null(color_scaling)){
    color_scaling <- 'rse'
  }

  # Create compartments -----------------------------------------------------
  comp_data  <- define_comp_layout(qmd_info, color_scaling = color_scaling, ...)
  if(is.integer(rank) || is.numeric(rank)) {
    if(length(rank) != nrow(comp_data)) {
      msg(paste0('Provide an integer rank for each of the following compartment\n',
                 paste(comp_data$label, collapse =', '), ':'), TRUE)
      rank <- readline(prompt = '')
      rank <- as.numeric(unlist(strsplit(rank, '\\D+')))
    }
    comp_data$rank <- rank
  }


  # Create arrows -----------------------------------------------------------
  arrow_data <- define_arrow_layout(qmd_info, color_scaling = color_scaling, ...)



  # PBPK layout -------------------------------------------------------------
  if(pbpk_layout) {
    pbpk_data  <- define_pbpk_layout(comp  = comp_data,
                                     arrow = arrow_data, ...)
  } else {
    pbpk_data  <- NULL
  }

  # Create graph ------------------------------------------------------------
  graph      <- define_graph(comp    = comp_data,
                             arrow   = arrow_data,
                             pbpk    = pbpk_data,
                             graph_attrs = c('splines = true', # ortho for square
                                             'ranksep = 0', # Change for PBPK scaled
                                             'nodesep = 0.15',
                                             ifelse(flipped,
                                                    'rankdir = TB',
                                                    'rankdir = LR')))

  # Render graph ------------------------------------------------------------
  if(shiny) {
    DiagrammeR::grViz(graph$dot_code)
  } else {
    DiagrammeR::render_graph(graph,
                             output = output,
                             width  = width,
                             height = height)
  }
} # End qmd
