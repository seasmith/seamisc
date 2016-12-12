
# add_and() ---------------------------------------------------------------
#
#' Add 'and' to the second-to-last position of a character vector for convenient
#' printing when using knitr to display data in an Rmd document.
#'
#' @param string A character vector or a factor vector. Factors will be
#'   converted to character.
add_and <- function(string, and = c("and", "&")) {
  stopifnot(is.character(string) | is.factor(string))
  if (is.factor(string)) as.character(string)
  len <- length(string)
  new.string <- c(string[1:(len -1)], and[1], string[len])
  return(new.string)
}


# paste() -----------------------------------------------------------------
#
#' Paste unqouted values and return unqouted name/symbol
paste00 <- function(..., sep = "", collapse = NULL){
    dots <- as.list(substitute(list(...)))[-1L]
    dots.pasted <- paste(dots, sep = sep, collapse = collapse)
    dots.name <- as.name(dots.pasted)
}

# camelize() --------------------------------------------------------------
#
#' Convert snake_case to CamelCase.
#'
#' @param x Name of function/object to be converted from snake to camel.
#' @param first Logical.
#' @seealso
#' Taken from ~/ggplot2/utilities.r.
#' Taken from ~/ggplot2/utilities.r
#' @export

camelize <- function(x, first = FALSE) {
  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
  if (first) x <- firstUpper(x)
  x
}



# snakeize() --------------------------------------------------------------
#
#' Convert CamelCase to snake_case.
#'
#' @param x Name of function/object to be converted from camel to snake.
#'
#' @seealso
#' Taken from ~/ggplot2/utilities.r.
#' Taken from ~/ggplot2/utilities.r
#' @export

snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  tolower(x)
}
