#' Automated creation and rendering of QMD graphs
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
#' @param parse_labels logical if \code{TRUE} arrow labels will be parsed to use subscript the rate
#' constant name (\deqn{KA} will become \deqn{K_A}) and the inter-compartmental clearance number
#' (\deqn{Q2} would become \deqn{Q_2})
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
#' @param rank integer vertor assigning a rank to each compartment. Can be used
#' to obtain a specific layout. The ranks must be greater or equal to 1.
#' @param title A title to be added to the graph
#' @param clearance_mode logical if \code{TRUE} clearances will be represented by triangles
#'  and their surface area will be proportional to the volume cleared per unit of time
#' @param pbpk_layout logical if \code{TRUE} a PBPK layout will be applied
#' @param vein_comp_label label of the veinous compartment
#' @param artery_comp_label label of the arterial compartment
#' @param save_qmd save the QMD graph into a file (default \code{FALSE})
#' @param file_name name of the file to be created on the disk when save_qmd is \code{TRUE}
#' @param format file format when save_qmd is \code{TRUE}. Must be one of "png", "pdf", "svg" or "ps"
#' @param width width of the resulting graphic in pixels
#' @param height height of the resulting graphic in pixels
#' @param graph_attrs	an optional data.frame of graph attribute statements that can
#' serve as defaults for the graph.
#'
#' @seealso \code{\link{import_qmd_info}}, \code{\link{format_qmd_info}}
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
                parse_labels       = FALSE,
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
                title              = NULL,
                clearance_mode     = FALSE,
                pbpk_layout        = FALSE,
                vein_comp_label    = 'venous',
                artery_comp_label  = 'arterial',
                save_qmd           = FALSE,
                file_name          = NULL,
                format             = 'svg',
                width              = NULL,
                height             = NULL,
                graph_attrs        = NULL) {

  # Check inputs ------------------------------------------------------------
  if (is.null(qmd_info)) {
    stop('Argument \"qmd_info\" required.')
  }

  # Create compartments -----------------------------------------------------
  comp_data  <- define_comp_layout(qmd_info,
                                   scaling           = scaling,
                                   rank              = rank,
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


  # Create arrows -----------------------------------------------------------
  arrow_data <- define_arrow_layout(qmd_info,
                                    scaling            = scaling,
                                    arrow_scale_fun    = arrow_scale_fun,
                                    clearance_mode     = clearance_mode,
                                    color_scaling      = color_scaling,
                                    color_cutoff       = color_cutoff,
                                    labels             = labels,
                                    parse_labels       = parse_labels,
                                    alpha              = alpha,
                                    arrow_color_manual = arrow_color_manual,
                                    unscaled_color     = unscaled_color,
                                    font               = font,
                                    arrow_fontsize     = arrow_fontsize)


  # PBPK layout -------------------------------------------------------------
  if (pbpk_layout) {
    pbpk_data  <- define_pbpk_layout(comp              = comp_data,
                                     arrow             = arrow_data,
                                     pbpk_color        = ifelse(toupper(color_scaling) == 'PBPK', TRUE, FALSE),
                                     vein_comp_label   = vein_comp_label,
                                     artery_comp_label = artery_comp_label)
  } else {
    pbpk_data  <- NULL
  }


  # Create QMD graph --------------------------------------------------------
  create_qmd(comp        = comp_data,
             arrow       = arrow_data,
             pbpk        = pbpk_data,
             graph_attrs = graph_attrs,
             flipped     = flipped,
             title       = title,
             save_qmd    = save_qmd,
             file_name   = file_name,
             format      = format,
             width       = width,
             height      = height)

} # End qmd
