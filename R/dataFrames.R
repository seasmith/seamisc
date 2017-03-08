#' @include utils.dataFrames.R
NULL



# graph_miss_col() ------------------------------------------------------------

graph_miss_col <- function(x) {
  nms <- names(x)
  ncl <- ncol(x)
  nrw <- nrow(x)
  
  df_edges <- data.frame(parent = c(rep("!NA", ncl), rep("NA", ncl), "data", "data"),
                         child  = c(paste0(nms, "_!NA"), paste0(nms, "_NA"), "!NA", "NA"))
  # data.frame(parent = c(rep("data", ncl*2)),
  #                         child  = c(paste0(nms, "_!NA"), paste0(nms, "_NA")))
  
  
  nms_rep <- vapply(nms, rep, character(2), 2)
  nms_rep <- as.character(nms_rep)
  
  nms_apnd <- vapply(nms,
                     function(x) c(paste0(x, "_!NA"), paste0(x, "_NA")),
                     character(2))
  nms_apnd <- as.character(nms_apnd)
  
  df_edges2 <- tibble::tibble(parent = c(rep("data", ncl),
                            nms_rep),
                 child  = c(nms,
                            nms_apnd))
  
  # Create missing data summary
  df_miss <- naniar::miss_var_summary(x)
  df_miss <- dplyr::select(df_miss, -percent)
  df_miss <- dplyr::rename(df_miss, size = n_missing)

  # Create present data summary
  df_pres <- dplyr::tibble(variable = df_miss$variable,
                              size     = nrw - df_miss$size)

  # Remove 0's
  df_miss    <- dplyr::filter(df_miss, size != 0)
  df_pres <- dplyr::filter(df_pres, size != 0)
  
  # Append appropriate modifiers to the end of variables
  df_miss <- dplyr::mutate(df_miss, col = variable, variable = paste0(variable, "_NA"))
  df_pres <- dplyr::mutate(df_pres, col = variable, variable = paste0(variable, "_!NA"))
  
  # Add summary row for each table
  df_miss <- dplyr::add_row(.data    = df_miss,
                            variable = "NA",
                            size     = 0)
  df_pres <- dplyr::add_row(.data       = df_pres,
                               variable = "!NA",
                               size     = 0)
  
  nrw_miss <- nrow(df_miss)
  nrw_pres <- nrow(df_pres)
  
  df_miss <- tibble::add_column(.data = df_miss,
                                value = rep("NA", nrw_miss))
  df_pres <- tibble::add_column(.data = df_pres,
                                value = rep("!NA", nrw_pres))
  
  df_vertices <- dplyr::union(df_miss, df_pres)

  df_vertices <- dplyr::add_row(.data    = df_vertices,
                                variable = "data",
                                size     = 0,
                                value = "data")
  
  df_vertices2 <- dplyr::filter(df_vertices, variable != "!NA")
  df_vertices2 <- dplyr::filter(df_vertices2, variable != "NA")
  df_vertices2 <- dplyr::add_row(.data    = df_vertices2,
                                 variable = nms,
                                 size     = 0,
                                 col      = NA,
                                 value    = NA)
  df_vertices2 <- dplyr::semi_join(df_edges2, df_vertices2, by = c("child" = "variable"))
  
  df_edges2 <- dplyr::semi_join(df_edges2, df_vertices2, by = c("child" = "variable"))

  graph <- igraph::graph_from_data_frame(df_edges, vertices = df_vertices)
  
  graph2 <- igraph::graph_from_data_frame(df_edges2, vertices = df_vertices2)

  graphs <- lapply(list(graph, graph2), function(x) {
    x <- ggraph::tree_apply(x, function(node, parent, depth, tree) {
      tree <- igraph::set_vertex_attr(tree, 'depth', node, depth)
      if (depth == 1) {
        tree <- igraph::set_vertex_attr(tree, 'class', node, igraph::V(tree)$value[node])
      } else if (depth > 1) {
        tree <- igraph::set_vertex_attr(tree, 'class', node, igraph::V(tree)$class[parent])
      }
      tree
    })
    
  })
  
  names(graphs) <- c("by_presence", "by_column")
  
  igraph::V(graphs$by_presence)$leaf <- igraph::degree(graphs$by_presence, mode = 'out') == 0
  igraph::V(graphs$by_column)$leaf <- igraph::degree(graphs$by_column, mode = 'out') == 0

}



# gg_miss_treemap() -------------------------------------------------------

gg_miss_treemap <- function(graphs) {
  ggraph(graphs$by_presence, 'treemap', weight = "size", width = 1, height = 3) +
    geom_node_tile(aes(filter = leaf, fill = forcats::fct_rev(class)), colour = NA) +
    geom_node_tile(aes(filter = depth != 0, size = depth), fill = NA) +
    geom_node_text(aes(label = col), repel = TRUE) +
    scale_alpha(range = c(1, 0.3), guide = 'none') +
    scale_size(range = c(1.5, 0.4), guide = 'none')
  
  ggraph(graphs$by_column, 'treemap', weight = "size", width = 1, height = 3) +
    geom_node_tile(aes(filter = leaf, fill = forcats::fct_rev(value)), colour = NA) +
    geom_node_tile(aes(filter = depth != 0, size = depth), fill = NA) +
    geom_node_text(aes(label = col)) +
    scale_alpha(range = c(1, 0.3), guide = 'none') +
    scale_size(range = c(1.5, 0.4), guide = 'none') + coord_flip()
  
}




# graph_miss_cat() --------------------------------------------------------

graph_miss_cat <- function(x, ...) {
  dots <- as.list(substitute(list(...)))[-1L]
  chars <- as.character(dots)
  len <- length(chars)
  
  # x %>%
  #   split(.[, dots]) %>%
  #   map(naniar::miss_var_summary) %>%
  #   plyr::ldply()  
  
  x %>%
    group_by_(groups = chars) %>%
    dmap(function(x) sum(is.na(x))) %>%
    gather(... = -groups, "variable", "n_missing") %>%
    mutate(percent = (n_missing / nrow(x) * 100)) %>%
    arrange(-n_missing)
}





# at() --------------------------------------------------------------------

#' Wrapper for \code{tibble::as_tibble()}.
#' 
#' @param x A data frame to convert to a tibble.
#' 
#' @export
at <- function(x) {
  tibble::as_tibble(x)
}

# getColNums() ------------------------------------------------------------ '
#'
#' Returns columns numbers using dplyr-like selection syntax (i.e. col1:col5).
#'
#' @param .data Object that has named indexes. Only data frames are supported at
#'   this time.
#' @param ... Unquoted index names (i.e. column names).
#' @rdname getColNums
#' @export


getColNums <- function(.data, ...) {
  # UseMethod("getColNums")
  expr <- as.list(substitute(list(...)))[-1L]

  vars       <- names(.data)
  names_list <- stats::setNames(as.list(seq_along(vars)), vars)

  eval.index <- function(expr, names_list) {
    ndx <- lazyeval::lazy_dots(eval(expr)) %>%
      lazyeval::as.lazy_dots() %>%
      lazyeval::lazy_eval(c(names_list, select_helpers))
    return(ndx)
  }
  colNums <- sapply(seq_along(expr), function(x) {
    eval.index(expr[[x]], names_list)
  })

  return(unlist(colNums))
}



# rm_commas() -------------------------------------------------------------
# Not ready for prime-time, yet.

# rm_commas <- function(df, cols) {
#   # cols <- sapply(cols, function(x) {
#   #   x <- quote(x)
#   #   x <- if (is.numeric(x) || is.integer(x)) return(x)
#   # })
#
#   lapply(cols, function(x) {
#     gsub(",", "", df[[x]])
#   })
# }



# myVLOOKUP() -------------------------------------------------------------
#' Implementation of an unsanitary program's infamous function.
#'
#' @param df A data frame to lookup values and return values.
#' @param lookupValue The value to be looked up.
#' @param lookupColumn The column in which to lookup the \code{lookupValue}.
#' @param returnColumn The column where values will be extracted for matches
#'   found in \code{lookupColumn}.
#' @export


myVLOOKUP <- function(df, lookupValue, lookupColumn, returnColumn) {
  df[df[, lookupColumn] == lookupValue, returnColumn]
}



# split_apply() -----------------------------------------------------------
#
#' Apply a function to a faceted piece of data.
#'
#' @param data A data frame.
#' @param x The independent variable (the one which \code{FUN} will be applied
#'   to).
#' @param y The dependent variable (the one which will facet or subset
#'   \code{data}).
#' @param FUN A function to apply to the faceted piece of data.
#'
#' @examples
#'
#' @export


split_apply <- function(data, x, y, FUN, SIMPLIFY = TRUE){
  data.split <- split(data, data[[y]])
  FUN.apply <- sapply(seq_along(data.split), function(i){
    FUN(data.split[[i]][[x]])
  })
  if(SIMPLIFY){
    FUN.result <- do.call(cbind, list(names(data.split), FUN.apply))
    return(FUN.result)
  } else{
    names(FUN.apply) <- names(data.split)
    return(FUN.apply)
  }
}

# #' @export
# facet_fun <- function(data, x, y, FUN){
#   data.split <- split(data, data[[y]])
#   # FUN.calc <- sapply(seq_along(data.split), function(i){
#   #     FUN(data.split[[i]][[x]])
#   # })
#   FUN.calc <- rapply(data.split, FUN)
#
#   # names(FUN.calc) <- names(data.split)
#   return(FUN.calc)
# }
