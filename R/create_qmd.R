#' Creation and rendering of QMD graphs
#'
#' @description Creates a QMD graph using data.frames for comp and arrow.
#'
#' @param comp a data.frame created by \code{define_comp_layout}
#' @param arrow a data.frame created by \code{define_arrow_layout}
#' @param pbpk a list created by \code{define_pbpk_layout}
#' @param graph_attrs	an optional data.frame of graph attribute statements that can
#' serve as defaults for the graph
#' @param flipped logical if \code{TRUE} the layout will be flipped
#' @param title a title to be added to the graph
#' @param save_qmd save the graph into a file (default \code{FALSE})
#' @param file_name name of the file to be created on the disk (without extension) when save_qmd is \code{TRUE}
#' @param format file format when save_qmd is \code{TRUE}. Must be one of "png", "pdf", "svg" or "ps"
#' @param width width of the resulting graphic in pixels
#' @param height height of the resulting graphic in pixels
#'
#' @examples
#' \dontrun{
#' define_graph(comp  = define_comp_layout(examples$onecomp),
#'              arrow = define_arrow_layout(examples$onecomp))
#' }
#' @export
create_qmd <- function(comp        = NULL,
                       arrow       = NULL,
                       pbpk        = NULL,
                       graph_attrs = NULL,
                       flipped     = FALSE,
                       title       = NULL,
                       save_qmd    = FALSE,
                       file_name   = NULL,
                       format      = 'svg',
                       width       = NULL,
                       height      = NULL) {

  # Check inputs ------------------------------------------------------------
  if (is.null(comp) && is.null(pbpk)) {
    stop('Argument \"comp\" or \"pbpk\" required.')
  }

  if (!is.null(graph_attrs) && !is.data.frame(graph_attrs)) {
    msg('Argument "graph_attrs\" must be a data.frame.', TRUE)
    graph_attrs <- NULL
  } else if (is.data.frame(graph_attrs) &&
             !all(colnames(graph_attrs) %in% c('attr', 'value', 'type'))) {
    msg('The columns of the \"graph_attrs\" data.frame must match \"attr\", \"value\" and \"type\"', TRUE)
    graph_attrs <- NULL
  }

  # Create key variables ----------------------------------------------------
  if (!is.null(pbpk) && is.list(pbpk)) {
    nodes_df    <- pbpk$comp
    edges_df    <- pbpk$arrow
    pbpk_layout <- TRUE
  } else {
    nodes_df    <- comp
    edges_df    <- arrow
    pbpk_layout <- FALSE
  }

  # Add invisible nodes and arrows to force layout when using rank ----------
  # Create hidden nodes
  max_rank    <- max(nodes_df$rank, na.rm = TRUE)
  if (max_rank > 1) {
    hidden_ndf  <- DiagrammeR::create_node_df(n      = max_rank,
                                              rank   = 1:max_rank,
                                              style  = 'invis',
                                              shape  = 'point',
                                              width  = 0,
                                              heigth = 0)

    # Renumber nodes_df id and combine with hidden nodes
    nodes_df$id <- as.numeric(nodes_df$id) + max_rank
    nodes_df    <- DiagrammeR::combine_ndfs(hidden_ndf, nodes_df)

    # Create hidden edges
    hidden_edf  <- data.frame(from  = 2:max_rank - 1,
                              to    = 2:max_rank,
                              style = 'invis',
                              stringsAsFactors = FALSE)

    # Renumber edges_df connections and combine with hidden edges
    edges_df$from <- as.numeric(edges_df$from) + max_rank
    edges_df$to   <- as.numeric(edges_df$to) + max_rank
    edges_df <- DiagrammeR::combine_edfs(hidden_edf, edges_df)
  }

  # create graph ------------------------------------------------------------
  graph <- DiagrammeR::create_graph(nodes_df = nodes_df,
                                    edges_df = edges_df)

  # Set graph attributes ----------------------------------------------------
  if (is.null(graph_attrs)) {
    graph_attrs <- data.frame(attr  = c('layout', 'rankdir', 'ranksep', 'nodesep', 'splines'),
                              value = c('dot', ifelse(flipped, 'TB', 'LR'),
                                        ifelse(pbpk_layout, '0.25', '0'),
                                        ifelse(pbpk_layout, '0.25', '0.15'),
                                        ifelse(pbpk_layout, 'true', 'polyline')),
                              type  = 'graph')

  }

  graph <- DiagrammeR::set_global_graph_attrs(graph     = graph,
                                              attr      = graph_attrs[, 'attr'],
                                              value     = graph_attrs[, 'value'],
                                              attr_type = graph_attrs[, 'type'])

  # Render graph ------------------------------------------------------------
  if (save_qmd) {
    DiagrammeR::export_graph(graph,
                             file_name = file_name,
                             file_type = format,
                             width     = width,
                             height    = height)
  } else {
    DiagrammeR::render_graph(graph,
                             output = 'graph',
                             title  = title,
                             width  = width,
                             height = height)
  }
}
