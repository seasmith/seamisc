# recycle.NA() ------------------------------------------------------------
#
#' A vector recycler that recycles with \code{NA}'s.
#'
#' @param x A vector or a list of two vectors.
#' @param y A vector.

recycle.NA <- function(x, y){
  # Set x and y to a list and get length of each
  xy.list <- list(x = x, y = y)
  xy.lengths <- c(length.x = length(x), length.y = length(y))

  # Find which are max/min and return max length
  xy.max <- which.max(xy.lengths)
  xy.min <- which.min(xy.lengths)
  xy.max.length <- length(xy.list[[xy.max]])

  # Extend min component with NA's
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
  # Test if x is list; if TRUE then create separate x and y vectors
  if(is.list(x) && length(x) == 2){
    y <- x[[2]]
    x <- x[[1]]
  }

  # Set x and y to a list and get length of each
  xy.list <- list(x = x, y = y)
  xy.lengths <- c(length.x = length(x), length.y = length(y))

  # Find which are max/min
  xy.max <- which.max(xy.lengths)
  xy.min <- which.min(xy.lengths)

  # If both are same length then return the list
  if(xy.max == xy.min) return(xy.list)

  # Find the quotient and remainder; account for remainder == 0; set variables
  division <- xy.lengths[[xy.min]] %M% xy.lengths[[xy.max]]
  if.zero <- !is.na(division[["remainder"]]/division[["remainder"]]) %>% sum()
  xy.quotient <- rep(xy.list[[xy.min]], division[["quotient"]])
  xy.remainder <- rep(xy.list[[xy.min]][1L:division[["remainder"]]], if.zero)

  # Set min vector equal to quotient*vector + remainder
  xy.list[[xy.min]] <- c(xy.quotient, xy.remainder)

  return(xy.list)
}
