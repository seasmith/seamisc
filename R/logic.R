
# isFALSE() ---------------------------------------------------------------
#
#' This is an abbreviation of identical(FALSE, x) to go along with isTRUE()
#'
#' @param x An object to be tested if identical to FALSE.
#' @export


isFALSE <- function(x) identical(FALSE, x)


# `%||%`() ----------------------------------------------------------------
#
#' The replacement operator. Replaces the \code{lhs} with \code{rhs} on the
#' condition that \code{length(lhs) == FALSE} (the length is \code{0}).
#'
#' @param lhs An object of any length.
#' @param rhs A replacement value if  \code{length(lhs) == FALSE}.
#' @export


"%||%" <- function(lhs, rhs){
  if(length(lhs)) lhs else rhs
}
