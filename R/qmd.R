#' Automated creation and rendering of QMD graphs
#'
#' @description Create and renders Quantitative Model Diagrams (QMD)
#' for NONMEM models
#'
#' @inheritParams define_comp_layout
#' @inheritParams define_arrow_layout
#' @param flipped logical if \code{TRUE} the layout will be flipped
#' @param title A title to be added to the graph
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
                parse_labels       = TRUE,
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
  comp_data  <- define_comp_layout(qmd_info          = qmd_info,
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
  arrow_data <- define_arrow_layout(qmd_info           = qmd_info,
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
