#' @include utils.dataFrames.R
NULL

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
