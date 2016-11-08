
# `%M%`() -----------------------------------------------------------------
#
#' The modified combination of the modulus function (\code{\%\%}) and
#' integer divisor function (\code{\%/\%}).
#'
#' The placement of the arguments (\code{lhs} and \code{rhs}) does not matter
#' unlike the actual modulus function (\code{\%\%}) and integer divisor
#' function (\code{\%/\%})
#'
#' @param lhs A number (integer or numeric).
#' @param rhs A number (integer or numeric).
#' @export
`%M%` <- function(lhs, rhs){
    if(lhs < rhs){
        old.lhs <- lhs
        lhs <- rhs
        rhs <- old.lhs
    }
    x <- lhs %/% rhs
    y <- lhs %% rhs
    return(c(quotient = x, remainder = y))
}



