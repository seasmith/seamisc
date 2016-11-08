
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
#' # Gives the mean \code{wt} when \code{mtcars} has been faceted by \code{cyl}.
#' facet_fun(mtcars, "wt", "cyl", mean)
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

#' @export
facet_fun <- function(data, x, y, FUN){
  data.split <- split(data, data[[y]])
  # FUN.calc <- sapply(seq_along(data.split), function(i){
  #     FUN(data.split[[i]][[x]])
  # })
  FUN.calc <- rapply(data.split, FUN)

  # names(FUN.calc) <- names(data.split)
  return(FUN.calc)
}
