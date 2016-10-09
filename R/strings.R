
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
#' @seealso
#' Taken from ~/ggplot2/utilities.r.
camelize <- function(x, first = FALSE) {
  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
  if (first) x <- firstUpper(x)
  x
}



# snakeize() --------------------------------------------------------------
#
#' Convert CamelCase to snake_case.
#'
#' @seealso
#' Taken from ~/ggplot2/utilities.r.
snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  tolower(x)
}
