#' Create compartment layout
#'
#' @description Combine information for each compartment's rank, label, size,
#' color and scaling into a \code{data.frame}.
#'
#' @param qmd_info a \code{qmd_info} object generated with \code{import_qmd_info} or
#' \code{skeleton_qmd_info}
#' @param scaling logical if \code{TRUE} compartment size and colors will be scaled.
#'  If \code{FALSE} standard model diagram will be created
#' @param rank integer vertor assigning a rank for each compartment. Can be used
#' to obtain a specific layout
#' @param comp_scale_fun a function to be used for compartment size scaling
#' @param color_scaling can be 'iiv', 'rse', 'none' or 'pbpk'
#' @param color_cutoff numeric vector of length 2 setting the cutoff limits in color coding
#' for RSE (\%) or IIV (\%)
#' @param filled logical if \code{TRUE} compartment will be filled
#'  If \code{FALSE} only the compartment edges will be drawn
#' @param alpha transparency factor
#' @param comp_color_manual manually set color for each compartment
#' @param unscaled_color color of the unscaled compartments
#' @param unscaled_shape shape of the unscaled compartments. Can be square, circle or diamond
#' @param scaled_shape shape of the scaled compartments. Can be square, circle or diamond
#' @param font font name of the compartment label
#' @param comp_fontsize font size expansion factor
#'
#' @details The default \code{scaling_fun} argument is set to be a surface area
#' of the compartment.
#'
#' @seealso \code{\link{define_arrow_layout}}, \code{\link{import_qmd_info}}, \code{\link{qmd}}
#' @return A \code{data.frame}
#' @examples
#' \dontrun{
#' qmd_info <- import_qmd_info(dir = '../models/pk/', runno = '001')
#' comp     <- define_comp_layout(qmd_info)
#' }
#' @export
#'
define_comp_layout <- function(qmd_info           = NULL,
                               scaling            = TRUE,
                               rank               = NULL,
                               comp_scale_fun     = function(x) { sqrt(x) },
                               color_scaling      = 'RSE',
                               color_cutoff       = c(25, 50),
                               filled             = TRUE,
                               alpha              = 1,
                               comp_color_manual  = NULL,
                               unscaled_color     = NULL,
                               unscaled_shape     = 'circle',
                               scaled_shape       = 'square',
                               font               = 'Avenir',
                               comp_fontsize      = 1) {


  # Create key variables ----------------------------------------------------
  node <- qmd_info$parsed_comp

  # Dynamic QMDs
  if (toupper(color_scaling) == 'DQMD') {
    scaling <- TRUE
    dqmd    <- TRUE
  } else {
    dqmd    <- FALSE
  }

  # Check inputs ------------------------------------------------------------
  if (is.null(qmd_info)) {
    stop('Argument \"qmd_info\" required.')
  }

  if (is.null(node)) {
    stop('Level \"parsed_comp\" required in \"qmd_info\".')
  }

  if (qmd_info$advan == 10) {
    stop('ADVAN 10 is not currently supported by modelviz.')
  }

  if (!is.null(node) && !all(c('label', 'prm', 'output') %in% colnames(node))) {
    stop('Incorrect format of \"parsed_comp\" in \"qmd_info\", columns \"label\", \"prm\" and \"output\" required.')
  }

  if (scaling == FALSE) { node$prm <- NA }

  # Start node data creation ------------------------------------------------
  ## Set node ranking
  if (!is.null(rank)) {
    if (all(is.numeric(rank)) && length(rank) == nrow(node)) {
      node$rank <- rank
    } else {
      msg(paste0('Argument \"rank\" must be a numeric vector of length ', nrow(node), '.'), TRUE)
      node$rank  <- 1:nrow(node)
    }
  } else {
    node$rank  <- 1:nrow(node)
  }

  ## Create output compartment
  if (any(node$output)) {
    out_comp       <- node[node$output, ]
    out_comp$prm   <- NA
    out_comp$label <- paste0('Out_', out_comp$label)
    node           <- rbind(node, out_comp)
  }

  ## Create node data.frame template
  node$id    <- 1:nrow(node)
  node       <- node[, c('id', 'label', 'rank', 'prm')]
  node[, c('value', 'rse', 'iiv')] <- NA

  ## Ensure all prm exist
  if (all(is.na(node$prm))) {
    msg('Warning: No parameter provided in \"qmd_info$parsed_comp$prm\".', ifelse(scaling, TRUE, FALSE))
    scaling <- FALSE
  } else if (!all(node$prm[!is.na(node$prm)] %in% colnames(qmd_info$theta))) {
    msg('Warning: Wrong parameter label provided in \"qmd_info$parsed_comp$prm\".', TRUE)
    scaling <- FALSE
  }

  # Assign parameter values -------------------------------------------------
  if (scaling | toupper(color_scaling) == 'DQMD') {

    ### Add bioavailability scaling if possible
    if (!dqmd && any(grepl('^F\\d$', colnames(qmd_info$theta)))) {
      ba_prm  <- qmd_info$theta[1, grep('^F\\d$', colnames(qmd_info$theta)), drop = FALSE]
      ba_comp <- as.numeric(gsub('\\D', '', colnames(ba_prm)))

      if (!all(is.na(node$prm[node$id %in% ba_comp])) | any(ba_prm > 1)) {
        msg('Warning: Any of the F (e.g. F1) parameters were either not accessible or >1.', TRUE)
      } else {
        node$prm[node$id %in% ba_comp] <- paste0('F', node$id[node$id %in% ba_comp])
      }
    }

    ### Assign all prm, rse, iiv
    node[!is.na(node$prm), c('value', 'rse')] <- t(qmd_info$theta[, node[!is.na(node$prm), 'prm']])

    ### iiv labels assumed to have properly been cleaned in parse_ext_file
    if (length(intersect(colnames(qmd_info$omega), node$prm)) > 0) {
      node$iiv[which(node$prm %in% colnames(qmd_info$omega))] <-
        t(qmd_info$omega)[match(node$prm[which(node$prm %in% colnames(qmd_info$omega))],
                                colnames(qmd_info$omega)),1]
      node$iiv[node$iiv == 0] <- NA
    }
  }

  # Format nodes ------------------------------------------------------------
  ## Special variables
  comp_scale    <- ifelse(scaling, 0.2 , 1) # Reduce entire graph size due to issues with big arrows in graphviz
  if (dqmd) { comp_scale <- 0.5 }
  comp_fontsize <- comp_scale * comp_fontsize * 15.5      # Base size is 15.5
  node$fixedsize <- TRUE                                  # Forces nodes to respect defined size

  ## Scaling factor
  if (scaling & !dqmd) {
    node$scale[!is.na(node$value) & !grepl('^F\\d$', node$prm)] <- comp_scale_fun(node$value[!is.na(node$value) & !grepl('^F\\d$', node$prm)])
    node$width  <- ifelse(!is.na(node$scale), node$scale, 1)
  } else {
    node$scale  <- NA
    node$width  <- 1
  }

  ## Fonts
  node$fontsize <- comp_fontsize * node$width
  node$fontname <- font

  ## Resolution adjustment
  node$width  <- node$width * comp_scale
  node$height <- node$width

  ## Colors
  if (!is.numeric(color_cutoff) || length(color_cutoff) != 2) {
    msg('Argument \"color_cutoff\" must be a numeric vector of length of 2, units are in %.', TRUE)
    color_scaling <- 'NONE'
  }

  if (is.null(unscaled_color)) {
    unscaled_color <- ifelse(filled, 'grey80', 'black')
  }

  if (!is.null(comp_color_manual) & !dqmd) {
    if (nrow(node[!grepl('Out_', node$label),]) %% length(comp_color_manual) > 0) {
      stop('Inapropriate \"comp_color_manual\" provided.')
    }
    node$color <- 'black'
    node$color[!grepl('Out_', node$label)] <- hex_color(comp_color_manual, alpha)

  } else if ( (scaling == FALSE & toupper(color_scaling) != 'DQMD') | toupper(color_scaling) %in% c('NONE', 'PBPK')) {
    node$color <- hex_color(unscaled_color, alpha)

  } else if ((toupper(color_scaling) == 'RSE' & all(is.na(node$rse))) |
             (toupper(color_scaling) == 'IIV' & all(is.na(node$iiv)))) {
    msg(paste('Warning: Not enough information available on', color_scaling, 'for color scaling.'), TRUE)
    node$color <- hex_color(unscaled_color, alpha)

  } else if (toupper(color_scaling) == 'RSE') {
    node$color[is.na(node$rse)]              <- hex_color(unscaled_color, alpha) # light grey
    node$color[node$rse <= color_cutoff[1]]  <- hex_color('#B2E680', alpha)      # green
    node$color[node$rse > color_cutoff[1] &
                 node$rse < color_cutoff[2]] <- hex_color('#FFA366', alpha)      # orange
    node$color[node$rse > color_cutoff[2]]   <- hex_color('#FF8080', alpha)      # red

  } else if (toupper(color_scaling) == 'IIV') {
    node$color[is.na(node$iiv)]              <- hex_color(unscaled_color, alpha)   # light grey
    node$color[node$iiv <= color_cutoff[1]]  <- hex_color('#93D4EA', alpha)        # light blue
    node$color[node$iiv > color_cutoff[1] &
                 node$iiv < color_cutoff[2]] <- hex_color('#519BB4', alpha)        # blue
    node$color[node$iiv > color_cutoff[2]]   <- hex_color('#5471B0', alpha)        # dark blue
  } else if (dqmd) {
    node$color <- hex_color(unscaled_color, alpha) # light grey # Need more code safety
    node$scale <- 0
    node$scale[!is.na(node$value)] <- comp_scale_fun(node$value[!is.na(node$value)])
  }

  ## Shapes
  node$shape <- scaled_shape
  node$shape[is.na(node$value) | grepl('^F\\d$', node$prm)] <- unscaled_shape

  ## Style
  if (filled | dqmd) {
    node$style     <- 'filled'
    node$fillcolor <- node$color
    node$fontcolor <- 'black'
  } else {
    node$style     <- 'solid'
    node$fontcolor <- node$color
  }

  ## dQMD
  if (dqmd) {
    if (!is.null(comp_color_manual)) {
      node$fillcolor[!is.na(node$value)] <- paste0(hex_color(comp_color_manual, alpha) , ';',
                                                   node$scale[!is.na(node$value)], ':',
                                                   hex_color('white', 1))
    } else {
      node$fillcolor[!is.na(node$value)] <- paste0(hex_color('#519BB4', alpha) , ';',
                                                   node$scale[!is.na(node$value)], ':',
                                                   hex_color('white', 1))
    }
    node$fillcolor[!is.na(node$value) & node$value == 0] <- hex_color('white', 1)
    node$gradientangle <- 90
  }

  node$style[grepl('Out_', node$label)] <- 'invisible'
  node$penwidth    <- ifelse(node$style %in% c('filled', 'invisible') & !dqmd, 0, ifelse(scaling, 1, 2))

  ## Bioavailability
  if (any(grepl('^F\\d$', node$prm))) {
    node$style[grepl('^F\\d$', node$prm)] <- ifelse(unscaled_shape %in% c('circle', 'oval', 'ellipse'),
                               'wedged', 'striped')
    node$penwidth[grepl('^F\\d$', node$prm)] <- 1.5

    if (filled == FALSE) {
      node$fillcolor    <- hex_color('white', 1)
      node$fontcolor[grepl('^F\\d$', node$prm)] <- hex_color('black', 1)
    }
    node$fillcolor[grepl('^F\\d$', node$prm)]   <-
      paste0(node$color[grepl('^F\\d$', node$prm)], ';', node$value[grepl('^F\\d$', node$prm)], ':', hex_color('white', 1))
    node$color[grepl('^F\\d$', node$prm)] <- hex_color(node$color[grepl('^F\\d$', node$prm)], 1)
  }

  ## Tooltip
  node$tooltip[is.na(node$value)]  <- node$label[is.na(node$value)]
  node$tooltip[!is.na(node$value)] <- paste0(node$prm[!is.na(node$value)],' = ', signif(node$value[!is.na(node$value)], 4))

  if (!dqmd) {
    node$tooltip[!is.na(node$iiv)] <- paste0(node$tooltip[!is.na(node$iiv)], ' (', signif(node$iiv[!is.na(node$iiv)], 3), '% IIV)')
    node$tooltip[!is.na(node$rse)] <- paste0(node$tooltip[!is.na(node$rse)], ' [', signif(node$rse[!is.na(node$rse)], 3), '% RSE]')
  }

  return(node)
} # End define_comp_layout
