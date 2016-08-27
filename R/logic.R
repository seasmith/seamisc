
# isFALSE() ---------------------------------------------------------------
#
#' This is an abbreviation of identical(FALSE, x) to go along with isTRUE()
#'
#' Use this when needing to test explicitly if a value is FALSE.
#' @param x w or logical or ‘number-like’ vectors (i.e., of types double
#' (class numeric), integer and complex)), or objects for which methods have
#' been written.
#' @export

isFALSE <- function(x) identical(FALSE, x)


# `%||%`() ----------------------------------------------------------------
#
#' The replacement operator. Replaces the \code{lhs} with \code{rhs} on the
#' condition that \code{length(lhs) == FALSE} (the length is \code{0}).
#'
#' @param lhs An object of any length.
#' @param rhs A replacement value if  \code{length(lhs) == FALSE}.

`%||%` <- function(lhs, rhs){
  if(length(lhs)) lhs else rhs
}
