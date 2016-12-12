
# add_and() ---------------------------------------------------------------
#
#' Add 'and' to the second-to-last position of a character vector for convenient
#' printing when using knitr to display data in an Rmd document.
#'
#' @param string A character vector or a factor vector. Factors will be
#'   converted to character.
#' @param oxford Logical. Should the output string use the Oxford comma or not.
#' @param and Character vector indicating which style of "and" to use at the end
#'   of the output string. '\\b' indicates that there is to be no "and", which
#'   instead inserts a literal backspace.
#' @param sep Character vector indicating the separator to use. Could be the
#'   usual comma (","), semi-colon (";"), or something else.
#'
#' @export


add_and <- function(string, oxford = TRUE, and = c("and", "&", "\\b"), sep = c(",", ";")) {
  # Check - String must be a factor or character vector.
  stopifnot(is.character(string) | is.factor(string))
  if (is.factor(string)) as.character(string)

  # Inputs - Set length arguments.
  #        - Make paste easier by setting comma <- NULL if oxford == FALSE.
  #        - Set and <- "\\b" if there string is length 1 seq.len_1[1] == 0/FALSE
  len   <- length(string)
  len_1 <- len - 1
  seq.len_1 <- if (len_1) 1:len_1 else 0

  sep_space   <- paste0(sep[1], " ")
  sep_nospace <- if (oxford) sep[1] else NULL
  sep_nospace <- if (len > 2) sep_nospace else NULL

  and <- if (len == 1) "\\b" else and[1]

  # Input - First half of new string no matter the length or
  #       - if oxford == TRUE/FALSE.
  ns_first  <- paste(string[seq.len_1], collapse = sep_space)
  ns_second <- paste(sep_nospace, and, string[len])
  ns <- paste0(ns_first, ns_second)

  return(ns)
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
