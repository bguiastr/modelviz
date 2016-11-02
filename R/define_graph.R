#' Create a graph object using data frames representative of comp and arrow
#'
#' @description Generates a graph object using data frames for comp and arrow;
#' the graph object can be manipulated by other functions.
#'
#' @param comp a data.frame created by \code{define_comp_layout}
#' @param arrow a data.frame created by \code{define_arrow_layout}
#' @param pbpk a list created by \code{define_pbpk_layout}
#' @param graph_attrs	an optional vector of graph attribute statements that can
#' serve as defaults for the graph.
#' @param comp_attrs an optional vector of comp attribute statements that can
#' serve as defaults for comp.
#' @param arrow_attrs an optional vector of comp attribute statements that can
#' serve as defaults for arrow.
#' @param directed logical, if \code{TRUE} (default) directed arrow operations
#' will be generated.
#' @param graph_name an optional string for labeling the graph object.
#' @param graph_time a date or date-time string (required for insertion of graph
#' into a graph series of the type temporal).
#' @param graph_tz an optional value for the time zone (tz) corresponding to the
#' date or date-time string supplied as a value to graph_time. If no time zone is
#' provided then it will be set to GMT.
#'
#' @description Adapted from \code{DiagrammeR::create_graph} to include ranking and
#' follow notation used by modelviz.
#' @examples
#' \dontrun{
#' graph <- define_graph(comp  = define_comp_layout(examples$onecomp),
#'                       arrow = define_arrow_layout(examples$onecomp),
#'                       graph_attrs = c('splines = true',
#'                                       'ranksep = 0',
#'                                       'nodesep = 0.15',
#'                                       'rankdir = LR'))
#' DiagrammeR::render_graph(graph)
#' }
#' @importFrom DiagrammeR x11_hex
#' @export
define_graph <- function(comp        = NULL,
                         arrow       = NULL,
                         pbpk        = NULL,
                         graph_attrs = NULL,
                         comp_attrs  = NULL,
                         arrow_attrs = NULL,
                         directed    = TRUE,
                         graph_name  = NULL,
                         graph_time  = NULL,
                         graph_tz    = NULL) {

  # Equivalence to modelviz
  if (is.list(pbpk)) {
    nodes_df <- pbpk$comp
    edges_df <- pbpk$arrow
  } else {
    nodes_df <- comp
    edges_df <- arrow
  }
  node_attrs <- comp_attrs
  edge_attrs <- arrow_attrs

  # Code starts here
  graph_attributes <- c("bgcolor", "layout", "overlap", "fixedsize",
                        "mindist", "nodesep", "outputorder", "ranksep", "rankdir",
                        "stylesheet")
  node_attributes <- c("color", "distortion", "fillcolor",
                       "fixedsize", "fontcolor", "fontname", "fontsize", "group",
                       "height", "label", "labelloc", "margin", "orientation",
                       "penwidth", "peripheries", "pos", "shape", "sides",
                       "skew", "style", "tooltip", "width", "img", "icon", "gradientangle")
  edge_attributes <- c("arrowhead", "arrowsize", "arrowtail",
                       "color", "constraint", "decorate", "dir", "edgeURL",
                       "edgehref", "edgetarget", "edgetooltip", "fontcolor",
                       "fontname", "fontsize", "headclip", "headhref", "headlabel",
                       "headport", "headtarget", "headtooltip", "headURL",
                       "href", "id", "label", "labelangle", "labeldistance",
                       "labelfloat", "labelfontcolor", "labelfontname", "labelfontsize",
                       "labelhref", "labelURL", "labeltarget", "labeltooltip",
                       "layer", "lhead", "ltail", "minlen", "penwidth", "samehead",
                       "sametail", "style", "tailclip", "tailhref", "taillabel",
                       "tailport", "tailtarget", "tailtooltip", "tailURL",
                       "target", "tooltip", "weight")
  if (all(c(is.null(nodes_df), is.null(edges_df), is.null(graph_attrs),
            is.null(node_attrs), is.null(edge_attrs)))) {
    dot_code <- paste0(ifelse(directed == TRUE, "digraph",
                              "graph"), " {\n", "\n}")
    dgr_graph <- list(graph_name = graph_name, graph_time = graph_time,
                      graph_tz = graph_tz, nodes_df = NULL, edges_df = NULL,
                      graph_attrs = NULL, node_attrs = NULL, edge_attrs = NULL,
                      directed = ifelse(directed == TRUE, TRUE, FALSE),
                      dot_code = dot_code)
    attr(dgr_graph, "class") <- "dgr_graph"
    return(dgr_graph)
  }
  if (all(c(is.null(nodes_df), is.null(edges_df)))) {
    dot_code <- paste0(ifelse(directed == TRUE, "digraph",
                              "graph"), " {\n", "\n}")
    dgr_graph <- list(graph_name = graph_name, graph_time = graph_time,
                      graph_tz = graph_tz, nodes_df = NULL, edges_df = NULL,
                      graph_attrs = graph_attrs, node_attrs = node_attrs,
                      edge_attrs = edge_attrs, directed = ifelse(directed ==
                                                                   TRUE, TRUE, FALSE), dot_code = dot_code)
    attr(dgr_graph, "class") <- "dgr_graph"
    return(dgr_graph)
  }
  if (!is.null(nodes_df)) {
    stopifnot(any(c("node", "nodes", "node_id") %in% colnames(nodes_df)))
    for (i in 1:ncol(nodes_df)) {
      nodes_df[, i] <- as.character(nodes_df[, i])
    }
  }
  if (class(edges_df) == "data.frame") {
    if (ncol(edges_df) > 2) {
      stopifnot(any(c("from", "to") %in% colnames(edges_df)))
      for (i in 1:ncol(edges_df)) {
        edges_df[, i] <- as.character(edges_df[, i])
      }
    }
  }
  if (!is.null(graph_attrs)) {
    graph_attr_stmt <- paste0("graph [", paste(graph_attrs,
                                               collapse = ",\n       "), "]\n")
  }
  if (!is.null(node_attrs)) {
    node_attr_stmt <- paste0("node [", paste(node_attrs,
                                             collapse = ",\n     "), "]\n")
  }
  if (!is.null(edge_attrs)) {
    edge_attr_stmt <- paste0("edge [", paste(edge_attrs,
                                             collapse = ",\n     "), "]\n")
  }
  if (exists("graph_attr_stmt") & exists("node_attr_stmt") &
      exists("edge_attr_stmt")) {
    combined_attr_stmts <- paste(graph_attr_stmt, node_attr_stmt,
                                 edge_attr_stmt, sep = "\n")
  }
  if (!exists("graph_attr_stmt") & exists("node_attr_stmt") &
      exists("edge_attr_stmt")) {
    combined_attr_stmts <- paste(node_attr_stmt, edge_attr_stmt,
                                 sep = "\n")
  }
  if (exists("graph_attr_stmt") & !exists("node_attr_stmt") &
      exists("edge_attr_stmt")) {
    combined_attr_stmts <- paste(graph_attr_stmt, edge_attr_stmt,
                                 sep = "\n")
  }
  if (exists("graph_attr_stmt") & exists("node_attr_stmt") &
      !exists("edge_attr_stmt")) {
    combined_attr_stmts <- paste(graph_attr_stmt, node_attr_stmt,
                                 sep = "\n")
  }
  if (exists("graph_attr_stmt") & !exists("node_attr_stmt") &
      !exists("edge_attr_stmt")) {
    combined_attr_stmts <- paste0(graph_attr_stmt, "\n")
  }
  if (!exists("graph_attr_stmt") & exists("node_attr_stmt") &
      !exists("edge_attr_stmt")) {
    combined_attr_stmts <- paste0(node_attr_stmt, "\n")
  }
  if (!exists("graph_attr_stmt") & !exists("node_attr_stmt") &
      exists("edge_attr_stmt")) {
    combined_attr_stmts <- paste0(edge_attr_stmt, "\n")
  }
  if (!is.null(nodes_df)) {
    column_with_node_id <- which(colnames(nodes_df) %in%
                                   c("node_id", "node", "nodes"))[1]
    column_with_x <- which(colnames(nodes_df) %in% "x")[1]
    column_with_y <- which(colnames(nodes_df) %in% "y")[1]
    if (!is.na(column_with_x) & !is.na(column_with_y)) {
      pos <- data.frame(pos = paste0(nodes_df[, column_with_x],
                                     ",", nodes_df[, column_with_y], "!"))
      nodes_df <- cbind(nodes_df, pos)
    }
    if (any(grepl("$alpha^", colnames(nodes_df)))) {
      column_with_alpha_assigned <- grep("$alpha^", colnames(nodes_df))
    }
    else {
      column_with_alpha_assigned <- NA
    }
    if (!is.na(column_with_alpha_assigned)) {
      number_of_col_attr <- length(which(colnames(nodes_df) %in%
                                           c("color", "fillcolor", "fontcolor")))
      if (number_of_col_attr == 1) {
        name_of_col_attr <- colnames(nodes_df)[which(colnames(nodes_df) %in%
                                                       c("color", "fillcolor", "fontcolor"))]
        colnames(nodes_df)[column_with_alpha_assigned] <- paste0("alpha_",
                                                                 name_of_col_attr)
      }
    }
    if (any(grepl("alpha_.*", colnames(nodes_df)))) {
      alpha_column_no <- grep("alpha_.*", colnames(nodes_df))
      color_attr_column_name <- unlist(strsplit(colnames(nodes_df)[(which(grepl("alpha_.*",
                                                                                colnames(nodes_df))))], "_"))[-1]
      color_attr_column_no <- which(colnames(nodes_df) %in%
                                      color_attr_column_name)
      if (any(c("color", "fillcolor", "fontcolor") %in%
              colnames(nodes_df)[color_attr_column_no])) {
        if (all(grepl("[a-z]*", as.character(nodes_df[,
                                                      color_attr_column_no]))) & all(as.character(nodes_df[,
                                                                                                           color_attr_column_no]) %in% x11_hex()[, 1])) {
          for (i in 1:nrow(nodes_df)) {
            nodes_df[i, color_attr_column_no] <- paste0(x11_hex()[which(x11_hex()[,
                                                                                  1] %in% as.character(nodes_df[i, color_attr_column_no])),
                                                                  2], formatC(round(as.numeric(nodes_df[i,
                                                                                                        alpha_column_no]), 0), flag = "0", width = 2))
          }
        }
        if (all(grepl("#[0-9a-fA-F]{6}$", as.character(nodes_df[,
                                                                color_attr_column_no])))) {
          for (i in 1:nrow(nodes_df)) {
            nodes_df[, color_attr_column_no] <- as.character(nodes_df[,
                                                                      color_attr_column_no])
            nodes_df[i, color_attr_column_no] <- paste0(nodes_df[i,
                                                                 color_attr_column_no], round(as.numeric(nodes_df[i,
                                                                                                                  alpha_column_no]), 0))
          }
        }
      }
    }
    other_columns_with_node_attributes <- which(colnames(nodes_df) %in%
                                                  node_attributes)

    for (i in 1:nrow(nodes_df)) {
      if (i == 1)
        node_block <- vector(mode = "character", length = 0)
      if (length(other_columns_with_node_attributes) >
          0) {
        for (j in other_columns_with_node_attributes) {
          if (j == other_columns_with_node_attributes[1]) {
            attr_string <- vector(mode = "character",
                                  length = 0)
          }
          if (all(colnames(nodes_df)[j] %in% c("label",
                                               "tooltip"), nodes_df[i, j] == "")) {
            attribute <- NULL
          }
          else if (all(colnames(nodes_df)[j] %in% c("label",
                                                    "tooltip"), nodes_df[i, j] != "")) {
            attribute <- paste0(colnames(nodes_df)[j],
                                " = ", "'", nodes_df[i, j], "'")
          }
          else if (all(!(colnames(nodes_df)[j] %in%
                         c("label", "tooltip")), nodes_df[i, j] ==
                       "")) {
            attribute <- NULL
          }
          else if (all(!(colnames(nodes_df)[j] %in%
                         c("label", "tooltip")), nodes_df[i, j] !=
                       "")) {
            attribute <- paste0(colnames(nodes_df)[j],
                                " = ", "'", nodes_df[i, j], "'")
          }
          attr_string <- c(attr_string, attribute)
        }
        if (j == other_columns_with_node_attributes[length(other_columns_with_node_attributes)]) {
          attr_string <- paste(attr_string, collapse = ", ")
        }
      }
      if (exists("attr_string")) {
        line <- paste0("  '", nodes_df[i, column_with_node_id],
                       "'", " [", attr_string, "] ")
      }
      if (!exists("attr_string")) {
        line <- paste0("  '", nodes_df[i, column_with_node_id],
                       "'")
      }
      node_block <- c(node_block, line)
    }

    # Start modelviz edit -----------------------------------------------------
    if ('rank' %in% colnames(nodes_df)) {
      node_block <- tapply(node_block, nodes_df$rank, FUN = function(x){
        if (length(x) > 1) {
          x <- paste0('subgraph{rank = same\n',
                      paste0(x, collapse = '\n'),
                      '}\n')
        }
        return(x)
      })
    } # End Ranking

    node_block <- paste(node_block, collapse = "\n")

    # End modelviz edit -------------------------------------------------------


    if (exists("attr_string") == TRUE) {
      rm(attr_string)
    }
    if (exists("attribute") == TRUE) {
      rm(attribute)
    }
  } # End Nodes



  if (is.null(nodes_df) & !is.null(edges_df)) {
    from_to_columns <- ifelse(any(c("from", "to") %in% colnames(edges_df)),
                              TRUE, FALSE)
    other_columns_with_edge_attributes <- which(colnames(edges_df) %in%
                                                  edge_attributes)
    if (from_to_columns == TRUE) {
      both_from_to_columns <- all(c(any(c("from") %in%
                                          colnames(edges_df))), any(c("to") %in% colnames(edges_df)))
    }
    if (exists("both_from_to_columns")) {
      if (both_from_to_columns == TRUE) {
        from_column <- which(colnames(edges_df) %in%
                               c("from"))[1]
        to_column <- which(colnames(edges_df) %in% c("to"))[1]
      }
    }
    nodes_df <- as.data.frame(get_nodes(edges_df), stringsAsFactors = FALSE)
    colnames(nodes_df) <- "nodes"
    for (i in 1:length(nodes_df)) {
      if (i == 1)
        node_block <- vector(mode = "character", length = 0)
      node_block <- c(node_block, paste0("  '", nodes_df[i],
                                         "'"))
    }
    node_block <- paste(node_block, collapse = "\n")
  }
  if (!is.null(edges_df)) {
    from_to_columns <- ifelse(any(c("from", "to") %in% colnames(edges_df)),
                              TRUE, FALSE)
    other_columns_with_edge_attributes <- which(colnames(edges_df) %in%
                                                  edge_attributes)
    if (from_to_columns == TRUE) {
      both_from_to_columns <- all(c(any(c("from") %in%
                                          colnames(edges_df))), any(c("to") %in% colnames(edges_df)))
    }
    if (exists("both_from_to_columns")) {
      if (both_from_to_columns == TRUE) {
        from_column <- which(colnames(edges_df) %in%
                               c("from"))[1]
        to_column <- which(colnames(edges_df) %in% c("to"))[1]
      }
    }
    if (exists("from_column") & exists("to_column")) {
      if (length(from_column) == 1 & length(from_column) ==
          1) {
        for (i in 1:nrow(edges_df)) {
          if (i == 1)
            edge_block <- vector(mode = "character",
                                 length = 0)
          if (length(other_columns_with_edge_attributes) >
              0) {
            for (j in other_columns_with_edge_attributes) {
              if (j == other_columns_with_edge_attributes[1]) {
                attr_string <- vector(mode = "character",
                                      length = 0)
              }
              if (all(colnames(edges_df)[j] %in% c("edgetooltip",
                                                   "headtooltip", "label", "labeltooltip",
                                                   "taillabel", "tailtooltip", "tooltip"),
                      edges_df[i, j] == "")) {
                attribute <- NULL
              }
              else if (all(colnames(edges_df)[j] %in%
                           c("edgetooltip", "headtooltip", "label",
                             "labeltooltip", "taillabel", "tailtooltip",
                             "tooltip"), edges_df[i, j] != "")) {
                attribute <- paste0(colnames(edges_df)[j],
                                    " = ", "'", edges_df[i, j], "'")
              }
              else if (all(!(colnames(edges_df)[j] %in%
                             c("edgetooltip", "headtooltip", "label",
                               "labeltooltip", "taillabel", "tailtooltip",
                               "tooltip")), edges_df[i, j] == "")) {
                attribute <- NULL
              }
              else if (all(!(colnames(edges_df)[j] %in%
                             c("edgetooltip", "headtooltip", "label",
                               "labeltooltip", "taillabel", "tailtooltip",
                               "tooltip")), edges_df[i, j] != "")) {
                attribute <- paste0(colnames(edges_df)[j],
                                    " = ", "'", edges_df[i, j], "'")
              }
              attr_string <- c(attr_string, attribute)
            }
            if (j == other_columns_with_edge_attributes[length(other_columns_with_edge_attributes)]) {
              attr_string <- paste(attr_string, collapse = ", ")
            }
          }
          if (exists("attr_string")) {
            line <- paste0("'", edges_df[i, from_column],
                           "'", ifelse(directed == TRUE, "->", "--"),
                           "'", edges_df[i, to_column], "'", paste0(" [",
                                                                    attr_string, "] "))
          }
          if (!exists("attr_string")) {
            line <- paste0("  ", "'", edges_df[i, from_column],
                           "'", ifelse(directed == TRUE, "->", "--"),
                           "'", edges_df[i, to_column], "'", " ")
          }
          edge_block <- c(edge_block, line)
        }
      }
    }
    any_columns_with_edge_ops <- ifelse(any(c("edge_op",
                                              "edge_ops", "edge", "edges") %in% colnames(edges_df)),
                                        TRUE, FALSE)
    if (any_columns_with_edge_ops == TRUE) {
      column_with_edge_op <- which(colnames(edges_df) %in%
                                     c("edge_op", "edge_ops", "edge", "edges"))[1]
      directed_proportion <- sum(grepl("->", edges_df[,
                                                      column_with_edge_op]))/nrow(edges_df)
      directed <- ifelse(directed_proportion > 0.8, TRUE,
                         FALSE)
      for (i in 1:nrow(edges_df)) {
        if (i == 1)
          edge_block <- vector(mode = "character", length = 0)
        if (length(other_columns_with_edge_attributes) >
            0) {
          for (j in other_columns_with_edge_attributes) {
            if (j == other_columns_with_edge_attributes[1]) {
              attr_string <- vector(mode = "character",
                                    length = 0)
            }
            attribute <- paste0(colnames(edges_df)[j],
                                " = ", "'", edges_df[i, j], "'")
            attr_string <- c(attr_string, attribute)
          }
          if (j == other_columns_with_edge_attributes[length(other_columns_with_edge_attributes)]) {
            attr_string <- paste(attr_string, collapse = ", ")
          }
        }
        if (exists("attr_string")) {
          line <- paste0("  edge", " [", attr_string,
                         "] ", "'", gsub(" ", "", unlist(strsplit(edges_df[i,
                                                                           column_with_edge_op], "-[-|>]")))[1],
                         "'", ifelse(directed == TRUE, "->", "--"),
                         "'", gsub(" ", "", unlist(strsplit(edges_df[i,
                                                                     column_with_edge_op], "-[-|>]")))[2],
                         "'")
        }
        if (!exists("attr_string")) {
          line <- paste0("  '", gsub(" ", "", unlist(strsplit(edges_df[i,
                                                                       column_with_edge_op], "-[-|>]")))[1], "'",
                         ifelse(directed == TRUE, "->", "--"), "'",
                         gsub(" ", "", unlist(strsplit(edges_df[i,
                                                                column_with_edge_op], "-[-|>]")))[2],
                         "'")
        }
        edge_block <- c(edge_block, line)
      }
    }
    if (exists("edge_block")) {
      edge_block <- paste(edge_block, collapse = "\n")
    }
  }
  if (exists("combined_attr_stmts")) {
    if (exists("edge_block") & exists("node_block")) {
      combined_block <- paste(combined_attr_stmts, node_block,
                              edge_block, sep = "\n")
    }
    if (!exists("edge_block") & exists("node_block")) {
      combined_block <- paste(combined_attr_stmts, node_block,
                              sep = "\n")
    }
  }
  if (!exists("combined_attr_stmts")) {
    if (exists("edge_block")) {
      combined_block <- paste(node_block, edge_block,
                              sep = "\n")
    }
    if (!exists("edge_block")) {
      combined_block <- node_block
    }
  }
  dot_code <- paste0(ifelse(directed == TRUE, "digraph", "graph"),
                     " {\n", "\n", combined_block, "\n}")
  dot_code <- gsub(" \\[\\] ", "", dot_code)
  dgr_graph <- list(graph_name = graph_name, graph_time = graph_time,
                    graph_tz = graph_tz, nodes_df = nodes_df, edges_df = edges_df,
                    graph_attrs = graph_attrs, node_attrs = node_attrs,
                    edge_attrs = edge_attrs, directed = directed, dot_code = dot_code)
  attr(dgr_graph, "class") <- "dgr_graph"
  return(dgr_graph)
} # End define_graph
