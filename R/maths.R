
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



# facet_fun() -------------------------------------------------------------
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
facet_fun <- function(data, x, y, FUN){
    data.split <- split(data, data[[y]])
    FUN.calc <- sapply(seq_along(data.split), function(i){
        FUN(data.split[[i]][[x]])
    })
    names(FUN.calc) <- names(data.split)
    return(FUN.calc)
}

facet_fun2 <- function(data, x, y, FUN){
    data.split <- split(data, data[[y]])
    # FUN.calc <- sapply(seq_along(data.split), function(i){
    #     FUN(data.split[[i]][[x]])
    # })
    FUN.calc <- rapply(data.split, FUN)

    # names(FUN.calc) <- names(data.split)
    return(FUN.calc)
}
