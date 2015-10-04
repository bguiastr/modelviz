#' Create and render QMD
#'
#' @description Create and renders Quantitative Model Diagrams (QMD)
#' for NONMEM models
#'
#' @param qmd_info a \code{list} containing the parameters, their RSE and the
#'  nonmem subroutine ADVAN value
#' @param scaling logical, if \code{TRUE} arrow width and colors will be scaled.
#' If \code{FALSE} standard model diagram will be created
#' @param comp_scale_fun a function to be used for compartment size scaling
#' @param arrow_scale_fun a function to be used for arrow width scaling
#' @param labels logical if \code{TRUE} labels are added to arrows
#' @param font font name of the compartment label
#' @param comp_fontsize font size expansion factor
#' @param arrow_fontsize font size expansion factor
#' @param filled logical if \code{TRUE} compartment will be filled
#'  If \code{FALSE} only the compartment edges will be drawn
#' @param alpha transparency factor
#' @param color_scaling can be 'iiv', 'rse', 'none' or 'pbpk'
#' @param color_cutoff numeric vector of length 2 setting the cutoff limits in color coding
#' for RSE (\%) or IIV (\%)
#' @param arrow_color_manual manually set color for each arrow
#' @param comp_color_manual manually set color for each compartment
#' @param unscaled_color color of the unscaled compartments
#' @param unscaled_shape shape of the unscaled compartments. Can be square, circle or diamond
#' @param scaled_shape shape of the scaled compartments. Can be square, circle or diamond
#' @param flipped logical if \code{TRUE} the layout will be flipped
#' @param rank integer vertor assigning a rank for each compartment. Can be used
#' to obtain a specific layout
#' @param clearance_mode logical if \code{TRUE} clearances will be represented by triangles
#'  and their surface area will be proportional to the volume cleared per unit of time
#' @param pbpk_layout logical if \code{TRUE} a PBPK layout will be applied
#' @param vein_comp_label label of the veinous compartment
#' @param artery_comp_label label of the arterial compartment
#' @param shiny logical if \code{TRUE} output will be formated for shiny output
#' @param output format of the output to be returned by qmd ('graph', 'SVG', 'DOT' or 'vivagraph')
#' @param width width of the resulting graphic in pixels
#' @param height height of the resulting graphic in pixels
#' @param gv_options vector of options to be passed to Graphviz
#'
#' @seealso \code{\link{import_qmd_info}}, \code{\link{format_qmd_info}}
#' @return A graphic object
#' @examples
#' \dontrun{
#' qmd(qmd_info, scaling = FALSE)
#' }
#' @export
qmd <- function(qmd_info           = NULL,
                scaling            = TRUE,
                comp_scale_fun     = function(x) { sqrt(x) },
                arrow_scale_fun    = function(x) { x },
                labels             = TRUE,
                font               = 'Avenir',
                comp_fontsize      = 1,
                arrow_fontsize     = 1,
                filled             = TRUE,
                alpha              = 1,
                color_scaling      = 'RSE',
                color_cutoff       = c(25, 50),
                arrow_color_manual = NULL,
                comp_color_manual  = NULL,
                unscaled_color     = NULL,
                unscaled_shape     = 'circle',
                scaled_shape       = 'square',
                flipped            = FALSE,
                rank               = NULL,
                clearance_mode     = FALSE,
                pbpk_layout        = FALSE,
                vein_comp_label    = 'venous',
                artery_comp_label  = 'arterial',
                shiny              = FALSE,
                output             = 'graph',
                width              = NULL,
                height             = NULL,
                gv_options         = NULL) {

  # Check inputs ------------------------------------------------------------
  if(is.null(qmd_info)) {
    stop('Argument \"qmd_info\" required.')
  }

  # Create compartments -----------------------------------------------------
  comp_data  <- define_comp_layout(qmd_info,
                                   scaling           = scaling,
                                   comp_scale_fun    = comp_scale_fun,
                                   color_scaling     = color_scaling,
                                   color_cutoff      = color_cutoff,
                                   filled            = filled,
                                   alpha             = alpha,
                                   comp_color_manual = comp_color_manual,
                                   unscaled_color    = unscaled_color,
                                   unscaled_shape    = unscaled_shape,
                                   scaled_shape      = scaled_shape,
                                   font              = font,
                                   comp_fontsize     = comp_fontsize)

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
  arrow_data <- define_arrow_layout(qmd_info,
                                    scaling            = scaling,
                                    arrow_scale_fun    = arrow_scale_fun,
                                    clearance_mode     = clearance_mode,
                                    color_scaling      = color_scaling,
                                    color_cutoff       = color_cutoff,
                                    labels             = labels,
                                    alpha              = alpha,
                                    arrow_color_manual = arrow_color_manual,
                                    unscaled_color     = unscaled_color,
                                    font               = font,
                                    arrow_fontsize     = arrow_fontsize)


  # PBPK layout -------------------------------------------------------------
  if(pbpk_layout) {
    pbpk_data  <- define_pbpk_layout(comp              = comp_data,
                                     arrow             = arrow_data,
                                     pbpk_color        = ifelse(toupper(color_scaling) == 'PBPK', TRUE, FALSE),
                                     vein_comp_label   = vein_comp_label,
                                     artery_comp_label = artery_comp_label)
  } else {
    pbpk_data  <- NULL
  }


  # Create graph ------------------------------------------------------------
  ## Possibility to modify defaults
  if(is.null(gv_options)) {
    gv_options <- c(
      ifelse(flipped, 'rankdir = TB', 'rankdir = LR'),
      ifelse(pbpk_layout, 'ranksep = 0.5', 'ranksep = 0'),
      ifelse(pbpk_layout, 'nodesep = 0.25', 'nodesep = 0.15'))

  } else {
    gv_options <- c(
      if(!any(grepl('rankdir', gv_options))) {
        ifelse(flipped, 'rankdir = TB', 'rankdir = LR')
      },
      if(!any(grepl('ranksep', gv_options))) {
        ifelse(pbpk_layout, 'ranksep = 0.5', 'ranksep = 0')
      },
      if(!any(grepl('nodesep', gv_options))) {
        ifelse(pbpk_layout, 'nodesep = 0.25', 'nodesep = 0.15')
      },
      gv_options)
  }

  graph      <- define_graph(comp        = comp_data,
                             arrow       = arrow_data,
                             pbpk        = pbpk_data,
                             graph_attrs = gv_options)


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
