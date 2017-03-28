#' Create arrow connections between compartments
#'
#' @description Combine information for each arrow's origin, destination, label,
#' width, color and scaling into a \code{data.frame}.
#'
#' @param qmd_info a \code{qmd_info} object generated with \code{import_qmd_info} or
#' \code{skeleton_qmd_info}
#' @param scaling logical, if \code{TRUE} arrow width and colors will be scaled.
#' If \code{FALSE} standard model diagram will be created
#' @param arrow_scale_fun a function to be used for arrow width scaling
#' @param clearance_mode logical if \code{TRUE} clearances will be represented by triangles
#'  and their surface area will be proportional to the volume cleared per unit of time
#' @param color_scaling can be 'iiv', 'rse', 'none' or 'pbpk'
#' @param color_cutoff numeric vector of length 2 setting the cutoff limits in color coding
#' for RSE (\%) or IIV (\%)
#' @param labels logical if \code{TRUE} labels are added to arrows
#' @param parse_labels logical if \code{TRUE} labels will be parsed to use subscript the rate constant name (KA will become K subscript A)
#' and the inter-compartmental clearance number (Q2 would become Q subscript 2)
#' @param alpha transparency factor
#' @param arrow_color_manual manually set color for each arrow
#' @param unscaled_color color of the unscaled compartments
#' @param font font name of arrow labels
#' @param arrow_fontsize font size expansion factor

#' @details The default \code{scaling_fun} argument is set to scale the arrow width proportionally to the
#' square root of the parameter value, to match the method used on volumes.
#
#' @seealso \code{\link{define_comp_layout}}, \code{\link{import_qmd_info}}, \code{\link{qmd}}
#' @return A \code{data.frame}
#' @examples
#' \dontrun{
#' qmd_info <- import_qmd_info(dir = '../models/pk/', runno = '001')
#' arrow    <- define_arrow_layout(qmd_info)
#' }
#' @export
define_arrow_layout <- function(qmd_info           = NULL,
                                scaling            = TRUE,
                                arrow_scale_fun    = function(x) { x },
                                clearance_mode     = FALSE,
                                color_scaling      = 'RSE',
                                color_cutoff       = c(25, 50),
                                labels             = TRUE,
                                parse_labels       = FALSE,
                                alpha              = 1,
                                arrow_color_manual = NULL,
                                unscaled_color     = NULL,
                                font               = 'Avenir',
                                arrow_fontsize     = 1) {

  # Create key variables ----------------------------------------------------
  arrow <- qmd_info$parsed_arrow

  # Check inputs ------------------------------------------------------------
  if (is.null(qmd_info)) {
    stop('Argument \"qmd_info\" required.')
  }

  if (is.null(arrow)) {
    stop('Level \"parsed_arrow\" required in \"qmd_info\".')
  }

  if (qmd_info$advan == 10) {
    stop('ADVAN 10 is not currently supported by modelviz.')
  }

  if (!is.null(arrow) && !all(c('from', 'to', 'prm') %in% colnames(arrow))) {
    stop('Incorrect format of \"parsed_arrow\" in \"qmd_info\", columns \"from\", \"to\" and \"prm\" required.')
  }

  # Start arrow data creation ------------------------------------------------
  ## Ensure all prm exist
  if (length(arrow$prm[!is.na(arrow$prm)]) == 0) {
    msg('Warning: No parameter provided in \"qmd_info$parsed_arrow$prm\".', TRUE)
    scaling <- FALSE
  }

  ## Create arrow template structure
  if (labels && all(!is.na(arrow$prm))) {
    arrow$label <- ifelse(is.na(arrow$prm), '', arrow$prm)
  } else {
    arrow$label <- ''
    labels      <- FALSE
  }
  arrow[,c('value', 'rse', 'iiv')] <- NA

  # Assign parameter values -------------------------------------------------
  if (scaling) {
    arrow[!is.na(arrow$prm), c('value', 'rse')] <-
      t(qmd_info$theta[, colnames(qmd_info$theta) %in% arrow$prm[!is.na(arrow$prm)]])[
        match(arrow$prm[!is.na(arrow$prm)],
              rownames(t(qmd_info$theta[, colnames(qmd_info$theta) %in% arrow$prm[!is.na(arrow$prm)]]))),]

    ### iiv labels assumed to have properly been cleaned in parse_ext_file
    if (length(intersect(colnames(qmd_info$omega), arrow$prm)) > 0) {
      arrow$iiv[!is.na(arrow$prm)] <-
        t(qmd_info$omega[, colnames(qmd_info$omega) %in% arrow$prm[!is.na(arrow$prm)]])[
          match(arrow$prm[!is.na(arrow$prm)],
                rownames(t(qmd_info$omega[, colnames(qmd_info$omega) %in% arrow$prm[!is.na(arrow$prm)]]))), 1]
      arrow$iiv[arrow$iiv == 0] <- NA
    }
  }

  # Handle output compartment -----------------------------------------------
  if (any(is.na(arrow$to))) {
    if (is.null(qmd_info$parsed_comp)) {
      stop('Level \"parsed_comp\" required in \"qmd_info\".')
    }

    output_comp <- as.numeric(arrow$from[is.na(arrow$to)])
    if (all(qmd_info$parsed_comp[output_comp, 'output'])) {
      nodes <- nrow(qmd_info$parsed_comp) + (1:length(output_comp))
      arrow[is.na(arrow$to) ,'to'] <- nodes[order(output_comp)]
    } else {
      stop('Could not match output compartment with arrows.')
    }
  }

  # Format arrows ------------------------------------------------------------
  ## Special variables
  arrow_scale     <- ifelse(scaling, 0.4 , 1) # Reduce entire graph size due to issues with big arrows in graphviz
  arrow_fontsize  <- arrow_scale * arrow_fontsize * 15.5 # Base size is 15.5

  ## Scaling factor
  if (scaling) {
    arrow$scale[!is.na(arrow$value)] <- arrow_scale_fun(arrow$value[!is.na(arrow$value)])
    arrow$penwidth  <- ifelse(!is.na(arrow$scale), arrow$scale, 1)
    arrow$arrowsize <- ifelse(!is.na(arrow$scale), (arrow$scale*0.005)^0.23, 0.8)
  } else {
    arrow$scale     <- NA
    arrow$penwidth  <- 1
    arrow$arrowsize <- 0.8
  }

  ## Resolution adjustment
  arrow$penwidth <- arrow$penwidth * arrow_scale
  arrow$minlen   <- ifelse(scaling, 1, 2)

  ## Colors
  if (!is.numeric(color_cutoff) || length(color_cutoff) != 2) {
    msg('Argument \"color_cutoff\" must be a numeric vector of length of 2, units are in %.', TRUE)
    color_scaling <- 'NONE'
  }

  if (is.null(unscaled_color)) {
    unscaled_color <- ifelse(scaling == FALSE, 'black', 'grey80')
  }

  if (!is.null(arrow_color_manual)) {
    if (nrow(arrow) %% length(arrow_color_manual) > 0) {
      stop('Inapropriate \"arrow_color_manual\" provided.')
    }
    arrow$color <- hex_color(arrow_color_manual, alpha)

  } else if (scaling == FALSE | toupper(color_scaling) %in% c('NONE', 'PBPK')) {
    arrow$color <- hex_color(unscaled_color, alpha)

  } else if ((toupper(color_scaling) == 'RSE' & all(is.na(arrow$rse))) |
             (toupper(color_scaling) == 'IIV' & all(is.na(arrow$iiv)))) {
    msg(paste('Warning: Not enough information available on', color_scaling, 'for color scaling.'), TRUE)
    arrow$color <- hex_color(unscaled_color, alpha)

  } else if (toupper(color_scaling) == 'RSE') {
    arrow$color[is.na(arrow$rse)]              <- hex_color(unscaled_color, alpha)  # light grey
    arrow$color[arrow$rse <= color_cutoff[1]]  <- hex_color('#B2E680', alpha)       # green
    arrow$color[arrow$rse > color_cutoff[1] &
                  arrow$rse < color_cutoff[2]] <- hex_color('#FFA366', alpha)       # orange
    arrow$color[arrow$rse > color_cutoff[2]]   <- hex_color('#FF8080', alpha)       # red

  } else if (toupper(color_scaling) == 'IIV') {
    arrow$color[is.na(arrow$iiv)]              <- hex_color(unscaled_color, alpha)  # light grey
    arrow$color[arrow$iiv <= color_cutoff[1]]  <- hex_color('#93D4EA', alpha)       # light blue
    arrow$color[arrow$iiv > color_cutoff[1] &
                  arrow$iiv < color_cutoff[2]] <- hex_color('#519BB4', alpha)       # blue
    arrow$color[arrow$iiv > color_cutoff[2]]   <- hex_color('#5471B0', alpha)       # dark blue

  }

  ## Fonts
  arrow$fontsize  <- arrow_fontsize
  arrow$fontname  <- font
  arrow$fontcolor <- hex_color('black', alpha)

  ## Style
  arrow$style <- 'bold'
  if (scaling == TRUE) {
    arrow$style[is.na(arrow$value)] <- 'dotted'
  }

  ## Tooltip
  if (labels) {
    arrow$tooltip[is.na(arrow$value)]  <- arrow$label[is.na(arrow$value)]
  }

  if (any(!is.na(arrow$value))) {
    arrow$tooltip[!is.na(arrow$value)] <- paste0(arrow$prm[!is.na(arrow$value)],' = ',
                                                 signif(arrow$value[!is.na(arrow$value)], 4))
    arrow$tooltip[!is.na(arrow$iiv)]   <- paste0(arrow$tooltip[!is.na(arrow$iiv)], ' (',
                                                 signif(arrow$iiv[!is.na(arrow$iiv)], 3), '% IIV)')
    arrow$tooltip[!is.na(arrow$rse)]   <- paste0(arrow$tooltip[!is.na(arrow$rse)], ' [',
                                                 signif(arrow$rse[!is.na(arrow$rse)], 3), '% RSE]')
  }

  ## Parse labels
  if (labels && parse_labels) {
    # Ks
    Ks <- arrow$label[grep('^K+', arrow$label)]
    arrow$label[grep('^K+', arrow$label)] <- paste0('K@_{', substr(Ks, 2, nchar(Ks)), '}')

    # Qs
    Qs <- arrow$label[grep('^Q+', arrow$label)] # Restrict to '^Q\\d+' ?
    arrow$label[grep('^Q+', arrow$label)] <- paste0('Q@_{', substr(Qs, 2, nchar(Qs)), '}')
  }

  ## Attribute scale to NA arrows
  arrow$penwidth[is.na(arrow$scale)] <- min(arrow$penwidth, na.rm = TRUE)
  arrow$arrowsize[is.na(arrow$scale)] <- min(arrow$arrowsize, na.rm = TRUE)

  ## Required by DiagrammeR (v.9.1)
  arrow$rel <- NA

  return(arrow)
}
