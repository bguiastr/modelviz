#' Create compartment layout
#'
#' @description Combine information for each compartment's rank, label, size,
#' color and scaling into a \code{data.frame}.
#'
#' @param qmd_info a \code{qmd_info} object generated with \code{import_qmd_info} or
#' \code{skeleton_qmd_info}
#' @param scaling logical if \code{TRUE} compartment size and colors will be scaled.
#'  If \code{FALSE} standard model diagram will be created
#' @param comp_scale_fun a function to be used for compartment size scaling
#' @param color_scaling can be 'iiv', 'rse', 'none' or 'pbpk'
#' @param color_cutoff numeric vector of length 2 setting the cutoff limits in color coding
#' for RSE (\%) or IIV (\%)
#' @param filled logical if \code{TRUE} compartment will be filled
#'  If \code{FALSE} only the compartment edges will be drawn
#' @param alpha transparency factor
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
define_comp_layout <- function(qmd_info       = NULL,
                               scaling        = TRUE,
                               comp_scale_fun = function(x) { sqrt(x) },
                               color_scaling  = 'RSE',
                               color_cutoff   = c(25, 50),
                               filled         = TRUE,
                               alpha          = 1,
                               unscaled_color = NULL,
                               unscaled_shape = 'circle',
                               scaled_shape   = 'square',
                               font           = 'Avenir',
                               comp_fontsize  = 1,
                               ...) {


  # Create key variables ----------------------------------------------------
  parsed_comp <- qmd_info$parsed_comp

  # Check inputs ------------------------------------------------------------
  if(is.null(qmd_info)) {
    stop('Argument \"qmd_info\" required.')
  }

  if(is.null(parsed_comp)) {
    stop('Level \"parsed_comp\" required in \"qmd_info\".')
  }

  if(qmd_info$advan == 10) {
    stop('ADVAN 10 is not currently supported by modelviz.')
  }

  if(!is.null(parsed_comp) && !all(c('label', 'prm', 'output') %in% colnames(parsed_comp))){
    stop('Incorrect format of \"parsed_comp\" in \"qmd_info\", columns \"label\", \"prm\" and \"output\" required.')
  }

  if(scaling == FALSE) { parsed_comp$prm <- NA }

  # Create output compartment -----------------------------------------------
  if(any(parsed_comp$output)) {
    out_comp       <- parsed_comp[parsed_comp$output, ]
    out_comp$prm   <- NA
    out_comp$label <- paste0('Out_', out_comp$label)
    parsed_comp    <- rbind(parsed_comp, out_comp)
  }

  # Start node data creation ------------------------------------------------
  ## Create node data.frame template
  node <- as.data.frame(rep(NA, nrow(parsed_comp)) %o% rep(NA, 7))
  colnames(node) <- c('node', 'label', 'rank', 'prm', 'value', 'rse', 'iiv')

  ## Fill node and label rows
  node$node  <- paste0('A', 1:nrow(node))
  node$label <- parsed_comp$label

  ## Rank compartments
  node$rank <- 1:nrow(node)
  node$rank[grepl('^Out_', node$label)] <-
    node$rank[node$label %in% gsub('Out_', '', node$label[grepl('^Out_', node$label)], fixed = TRUE)]

  ## Ensure all prm exist
  if(length(parsed_comp$prm[!is.na(parsed_comp$prm)]) == 0) {
    msg('Warning: No parameter provided in \"qmd_info$parsed_comp$prm\".', ifelse(scaling, TRUE, FALSE))
    scaling <- FALSE
  }else if(!all(parsed_comp$prm[!is.na(parsed_comp$prm)] %in% colnames(qmd_info$theta))) {
    msg('Warning: Wrong parameter label provided in \"qmd_info$parsed_comp$prm\".', TRUE)
    scaling <- FALSE
  }


  # Assign parameter values -------------------------------------------------
  if(scaling) {
    ### Add F1 scaling if possible
    if('F1' %in% colnames(qmd_info$theta) && is.na(parsed_comp[1, 'prm']) && qmd_info$theta[1, 'F1'] < 1) {
      parsed_comp[1, 'prm'] <- 'F1'
    }

    ### Assign all prm, rse, iiv
    node$prm  <- parsed_comp$prm
    node[!is.na(node$prm), c('value', 'rse')] <- t(qmd_info$theta[, node[!is.na(node$prm), 'prm']])

    ### iiv labels assumed to have properly been cleaned in parse_ext_file
    if(length(intersect(colnames(qmd_info$omega), node$prm)) > 0) {
      node$iiv[which(node$prm %in% colnames(qmd_info$omega))] <-
        unlist(qmd_info$omega[1, intersect(colnames(qmd_info$omega), node$prm)])
    }
  }

  # Format nodes ------------------------------------------------------------
  ## Special variables
  comp_scale    <- ifelse(scaling, 0.2 ,1)                # Reduce entire graph size due to issues with big arrows in graphviz
  comp_fontsize <- comp_scale * comp_fontsize * 12        # Base size is 12
  node$fixedsize <- TRUE                                  # Forces nodes to respect defined size

  ## Scaling factor
  if(scaling) {
    node$scale[!is.na(node$value) & !grepl('F1', node$prm)] <- comp_scale_fun(node$value[!is.na(node$value) & !grepl('F1', node$prm)])
    node$width  <- ifelse(!is.na(node$scale), node$scale, 1)
  }else{
    node$scale  <- NA
    node$width  <- 1
  }

  ## Fonts
  node$fontsize <- comp_fontsize * node$width
  node$fontname <- font

  ## Resolution adjustment
  node$width <- node$width * comp_scale

  ## Colors
  if(length(color_cutoff) != 2) {
    msg('Argument \"color_cutoff\" must have length of 2, units are in %.', TRUE)
    color_cutoff <- c(25, 50)
  }

  if(is.null(unscaled_color)) {
    unscaled_color <- ifelse(filled, 'grey80', 'black')
  }

  if(scaling == FALSE | toupper(color_scaling) %in% c('NONE', 'PBPK')) {
    node$color <- hex_color(unscaled_color, alpha)

  } else if ((toupper(color_scaling) == 'RSE' & all(is.na(node$rse))) |
             (toupper(color_scaling) == 'IIV' & all(is.na(node$iiv)))) {
    msg(paste('Warning: Not enough information available on', color_scaling, 'for color scaling.'), TRUE)
    node$color <- hex_color(unscaled_color, alpha)

  } else if(toupper(color_scaling) == 'RSE'){
    node$color[is.na(node$rse)]              <- hex_color(unscaled_color, alpha) # light grey
    node$color[node$rse <= color_cutoff[1]]  <- hex_color('#B2E680', alpha)      # green
    node$color[node$rse > color_cutoff[1] &
                 node$rse < color_cutoff[2]] <- hex_color('#FFA366', alpha)      # orange
    node$color[node$rse > color_cutoff[2]]   <- hex_color('#FF8080', alpha)      # red

  } else if(toupper(color_scaling) == 'IIV'){
    node$color[is.na(node$iiv)]              <- hex_color(unscaled_color, alpha)   # light grey
    node$color[node$iiv <= color_cutoff[1]]  <- hex_color('#93D4EA', alpha)        # light blue
    node$color[node$iiv > color_cutoff[1] &
                 node$iiv < color_cutoff[2]] <- hex_color('#519BB4', alpha)        # blue
    node$color[node$iiv > color_cutoff[2]]   <- hex_color('#5471B0', alpha)        # dark blue
  }

  ## Shapes
  node$shape <- scaled_shape
  node$shape[is.na(node$value) | grepl('F1', node$prm)] <- unscaled_shape

  ## Style
  if(filled) {
    node$style     <- 'filled'
    node$fillcolor <- node$color
  } else{
    node$style     <- 'solid'
    node$fontcolor <- node$color
  }
  node$style[grepl('Out_', node$label)] <- 'invisible'
  node$penwidth    <- ifelse(node$style %in% c('filled', 'invisible'), 0, ifelse(scaling, 1, 2))

  ## Bioavailability
  if(any(grepl('F1', node$prm))) {
    node$style[1]    <- ifelse(unscaled_shape %in% c('circle','oval','ellipse'),
                               'wedged', 'striped')
    node$penwidth[1] <- 1.5

    if(filled == FALSE) {
      node$fillcolor    <- hex_color('white', 1)
      node$fontcolor[1] <- hex_color('black', 1)
    }
    node$fillcolor[1]   <-
      paste0(node$color[1], ';', node$value[1], ':', hex_color('white', 1))
    node$color[1] <- hex_color(node$color[1], 1)

  }

  ## Tooltip
  node$tooltip[is.na(node$value)]  <- node$label[is.na(node$value)]
  node$tooltip[!is.na(node$value)] <- paste0(node$prm[!is.na(node$value)],' = ', signif(node$value[!is.na(node$value)],4))
  node$tooltip[!is.na(node$iiv)]   <- paste0(node$tooltip[!is.na(node$iiv)], ' (', signif(node$iiv[!is.na(node$iiv)],3), ' %IIV)')
  node$tooltip[!is.na(node$rse)]   <- paste0(node$tooltip[!is.na(node$rse)], ' [', signif(node$rse[!is.na(node$rse)],3), ' %RSE]')

  return(node)
} # End define_comp_layout
