
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
