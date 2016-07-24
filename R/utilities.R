
# getFromFUN() ------------------------------------------------------------
#
#' Get all object names from a function's environment.
#'

getFromFUN <- function(x, value = FALSE){
  if(is.character(x)) x <- eval(as.name(x))

  e <- environment(x)
  ls.obj <- ls(envir = e, all.names = TRUE)

  if(value){
    ls.obj.names <- ls.obj
    ls.obj <- lapply(ls.obj, get0, envir = environment(x))
    names(ls.obj) <- ls.obj.names
  }

  return(ls.obj)
}

# expand.grid2() ----------------------------------------------------------
#
#' A new version of an old favorite with some extra options
#'
#' \code{expand.grid2()} creates a combination data frame from vectors or lists
#' but differs from the original \code{expand.grid()} in that it has two options
#' for removing two different type of duplicates. \code{stringsAsFactors} is
#' set to \code{TRUE}.
#'
#' @param ... Vectors, factors, or lists containing vectors.
#' @param rm.dupes Removes duplicated "rows". If \code{TRUE} (default) then
#' rows that are unordered duplicates of other rows will be removed. i.e.
#' \code{c("A", "B", "C")} is the same as \code{c("C", "B", "A")}
#'  and any other combination of \code{"A"}, \code{"B"}, and \code{"C"}.
#' @param rm.dubs Removes a row in which all elements are the same. If
#' \code{TRUE} (default) then a row such as \code{c("A", "A", "A")} will be
#' removed.

expand.grid2 <- function(..., rm.dupes = TRUE, rm.dubs = TRUE){
  nargs <- length(args <- list(...))

  # Eliminate duplicates within the vector
  args <- lapply(args, unique)
  grid <- expand.grid(args, stringsAsFactors = FALSE)
    grid.names <- names(grid)
    names(grid) <- NULL

  grid.list <- extract(grid, nrow(grid))

  # if(rm.dupes){
  #   is.dupes <- lapply(seq_along(grid.list), function(x){
  #     names(grid.list[[x]]) <- NULL
  #     grid.list[[x]]}) %>%
  #     lapply(sort) %>%
  #     duplicated()
  #   dupes <- if(sum(is.dupes)) which(is.dupes) else NULL
  # } else{
  #   dupes <- NULL
  # }

  if(rm.dupes){
    is.dupes <- lapply(grid.list, sort) %>%
      duplicated()
    dupes <- if(sum(is.dupes)) which(is.dupes) else NULL
  } else{
    dupes <- NULL
  }

  if(rm.dubs){
    is.dubs <- lapply(grid.list, duplicated) %>%
      lapply(sum) %>%
      `==`(nargs - 1)
    dubs <- if(sum(is.dubs)) which(is.dubs) else NULL
  } else{
    dubs <- NULL
  }

  deletes <- c(dupes, dubs)
  if(!is.null(deletes)) grid <- grid[-deletes, ]
  names(grid) <- grid.names

  return(grid)
}


