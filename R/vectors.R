# recycle.NA() ------------------------------------------------------------
#
#' A vector recycler that recycles with \code{NA}'s.
#'
#' @param x A vector or a list of two vectors.
#' @param y A vector.

recycle.NA <- function(x, y){
  xy.list <- list(x = x, y = y)
  xy.lengths <- c(length.x = length(x), length.y = length(y))
  xy.max <- which.max(xy.lengths)
  xy.min <- which.min(xy.lengths)
  xy.max.length <- length(xy.list[[xy.max]])

  xy.list[[xy.min]] <- xy.list[[xy.min]][1L:xy.max.length]
  return(xy.list)
}



# recycle.vector() --------------------------------------------------------
#
#' A vector recycler that recycles from the start of the vector.
#'
#' @param x A vector or a list of two vectors.
#' @param y A vector.

recycle.vector <- function(x, y){
  if(is.list(x) && length(x) == 2){
    y <- x[[2]]
    x <- x[[1]]
  }
  xy.list <- list(x = x, y = y)
  xy.lengths <- c(length.x = length(x), length.y = length(y))
  xy.max <- which.max(xy.lengths)
  xy.min <- which.min(xy.lengths)

  if(xy.max == xy.min) return(xy.list)

  division <- xy.lengths[[xy.min]] %M% xy.lengths[[xy.max]]
  if.zero <- !is.na(division[["remainder"]]/division[["remainder"]]) %>% sum()

  xy.quotient <- rep(xy.list[[xy.min]], division[["quotient"]])
  xy.remainder <- rep(xy.list[[xy.min]][1L:division[["remainder"]]], if.zero)

  xy.list[[xy.min]] <- c(xy.quotient, xy.remainder)

  return(xy.list)
}
