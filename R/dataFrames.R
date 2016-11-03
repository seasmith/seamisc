
# shittyVLOOKUP() ---------------------------------------------------------
#' Implementation of an unsanitary program's infamous function.
#'
#' @param df A data frame to lookup values and return values.
#' @param lookupValue The value to be looked up.
#' @param lookupColumn The column in which to lookup the \code{lookupValue}.
#' @param returnColumn The column where values will be extracted for matches
#'   found in \code{lookupColumn}.
#' @export
shittyVLOOKUP <- function(df, lookupValue, lookupColumn, returnColumn) {
  df[df[, lookupColumn] == lookupValue, returnColumn]
}
