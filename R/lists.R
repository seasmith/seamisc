
# list.pos() --------------------------------------------------------------
#'
#' Finds the position of a named list element within a list (with no
#' recurssion).
#'
#' @param  name A character vector. Ideally a character vector of length 1
#' (just one name); hoever it can accept a character vector of length greater
#' than 1. The names in the character vector will be used as names (element
#' headings) in the results vector.
#' @param lst A list. Ideally a list with all elements named.
#' @export
#'
#' @details
#' All elements in the input list must have a name for this function to give
#' accurate positions. This function can accept a character vector and return
#' the position of each name in the vector.
#'
#' Will return a character vector with names for each element corresponding
#' to the names in the character vector given to the function.If a name is not
#' present in the list then NA is returned.


list.pos <- function(name, lst){
    matches <- sapply(name, function(x){
        matched <- which(names(lst) %in% x)

        if(length(matched) == 0) matched <- NA
        matched
    })
    return(matches)
}



# extract() ---------------------------------------------------------------
#'
#' Sequentially extract elements from vectors of equal length in a list.
#'
#' @param lst A list of vectors of equal length, a data frame, or a matrix.
#' If the length of the smallest vector in \code{lst} is smaller than
#' \code{num} then an error will be thrown (\code{subscript out of bounds}).
#' @param num A number (preferably the length of the vectors) to create a
#' sequence for \code{extract()} to extract the elements of \code{lst}. Default
#' value is the length of the shortest vector in the list.
#' @export
#' @examples
#' # x <- list(a = 1:5, b = 6:9)
#' ## Does not extract last element in x$a; does not throw an error
#' # extract(x, length(x$b))
#' ## Throws an error because num (length(x$a)) is greater than the number of
#' ## elements in x$b (greater than length(x$b)).
#' #extract(x, length(x$a))
#'


extract <- function(lst, num = min(lengths(lst))){
    # if(!is.list(lst)) stop("Error: object lst is not a list")
    # extracted <- list()
    # for(i in seq_len(num)){
    #   # cl <- class(lst[[i]])
    #   # extracted[[i]] <- vapply(lst, `[[`, character(1), i)
    #   extracted[[i]] <- sapply(lst, `[[`, i)
    # }
    classes <- vapply(lst, class, character(1))
    # class.fun <- lapply(classes, match.fun)
    lst <- lapply(lst, as.character)
    extracted <- lapply(seq_len(num), function(i){
        vapply(lst, `[[`, character(1), i)
        # extracted[[i]] <- vapply(lst, `[[`, cl(1), i)
    })
    return(extracted)
}



# nestapply() -------------------------------------------------------------
#
#' Apply a function to a nested list.


nestapply <- function(obj, FUN, ...) {
  lapply(obj, function(i) {
    lapply(i, FUN, ...)
  })
}
