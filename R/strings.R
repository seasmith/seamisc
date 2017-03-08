
# ie() --------------------------------------------------------------------
#'
#' Write and format output to clipboard with preceeding "#> "; to be used in
#' notes within a markdown or some other document to express the console output
#' of an R process.
#' 
#' @param x Console output.
#' @export

 ie <- function(x) {

  x_eval <- eval(x, envir = parent.frame())
  print(x_eval)
  
  cap <- capture.output(x_eval)
  out <- cap %>% {paste("#>", .)} %>% writeClipboard()
  
}


# parse_API() -------------------------------------------------------------
#'
#' Parse a 10, 12, or 14-digit API well number.
#' 
#' @param x A vector of 10, 12, or 14-digit API well numbers or strings
#'   representing API numbers.
#' @export

parse_API <- function(x) {
  
  old_scipen <- getOption("scipen")
  options(scipen = 100)
  
  len <- stringi::stri_length(x)
  if (!all(len == 10 | len == 12 | len == 14)) {
    stop("Must have a valid 10 or 14 digit API number.")
  }
  
  state     <- stringi::stri_sub(x, 1L, 2L)
  county    <- stringi::stri_sub(x, 3L, 5L)
  uid       <- stringi::stri_sub(x, 6L, 10L)
  sidetrack <- stringi::stri_sub(x, 11L, 12L)
  event     <- stringi::stri_sub(x, 13L, 14L)
  
  options(scipen = old_scipen)
  
  return(list(state     = state,
              county    = county,
              uid       = uid,
              sidetrack = sidetrack,
              event     = event))
  
}


# num2char() --------------------------------------------------------------
#
#' Convert numeric numbers to character numbers.
#'
#' @param x A number or string coercible to a number.
#' @export

num2char <- function(x) {

  old_opt <- getOption("scipen")
  options(scipen = 100)

  if(!is.numeric(x) | !is.integer(x)) {

    alpha <- grep("^[[:digit:]]*$", x) %>% sum() < 1

    if (!alpha)
      x <- as.numeric(x) else
        stop("'x' must be numeric or integer or coercible to such.")

  }

  scale <- list(`0` = NULL,
                `1` = "thousand",
                `2` = "million",
                `3` = "billion",
                `4` = "trillion",
                `5` = "quadrillion",
                `6` = "quintillion",
                `7` = "sextillion",
                `8` = "septillion")

  len <- stringr::str_length(x)
  div_sets <- len %/% 3
  div_rem <- len %% 3


  it <- div_sets + if (div_rem != 0) 1 else 0
  i <- 3L
  j <- 1L
  trip_char <- NULL

  for(k in seq(it)) {

    set <- stringr::str_sub(x, start = -i, end = -j)

    trip <- triplet(set)
    trip_scale <- scale[[k]]

    trip_tmp <- if (is.null(trip)) NULL else paste(trip, trip_scale)
    trip_char <- paste(trip_tmp, trip_char)

    i <- i + 3L
    j <- j + 3L

  }

  trip_char <- trip_char %>% stringi::stri_trim()
  if (grepl("^-", trip_char)) trip_char <- stringi::stri_sub(trip_char, 2L)
  if (grepl("-$", trip_char)) trip_char <- stringi::stri_sub(trip_char, 1L, -2L)
  trip_char <- gsub("-\\s", " ", trip_char)
  trip_char <- gsub("\\s+", " ", trip_char)

  options(scipen = old_opt)
  return(trip_char)

}

#' @rdname num2char
triplet <- function(x) {

  single <- list(`1` = "one",
                 `2` = "two",
                 `3` = "three",
                 `4` = "four",
                 `5` = "five",
                 `6` = "six",
                 `7` = "seven",
                 `8` = "eight",
                 `9` = "nine")

  first_double <- list(`10` = "ten",
                       `11` = "eleven",
                       `12` = "twelve",
                       `13` = "thirteen",
                       `14` = "fourteen",
                       `15` = "fifteen",
                       `16` = "sixteen",
                       `17` = "seventeen",
                       `18` = "eighteen",
                       `19` = "nineteen")

  second_double <- list(`1` = NULL,
                        `2` = "twenty",
                        `3` = "thirty",
                        `4` = "forty",
                        `5` = "fifty",
                        `6` = "sixty",
                        `7` = "seventy",
                        `8` = "eighty",
                        `9` = "ninety")

  len <- stringr::str_length(x)
  parts <- len %>% seq() %>% rev() %>% lapply(function(i) {
    stringr::str_sub(x, i, i) %>% as.numeric()
  })

  first <- if (is.null(parts[3][[1]]))
    NULL else if (parts[3][[1]] == 0)
      NULL else
        paste0(single[parts[3][[1]]], "-", "hundred")

  second <- if (is.null(parts[2][[1]]))
    NULL else if (parts[2][[1]] == 0)
      NULL else if (parts[2][[1]] == 1)
        first_double[[parts[1][[1]] + 1]] else
          second_double[parts[2][[1]]]

  third <- if (is.null(parts[2][[1]]) & parts[1][[1]] != 0)
    single[parts[1][[1]]] else if(parts[2][[1]] != 1)
      single[parts[1][[1]]] else
        NULL

  hyph <- if (is.null(third))
    NULL else if(any(grepl(paste0("^", third, "$"), first_double)))
      NULL else if(is.null(second))
        NULL else
          "-"

  num_char <- paste(first, paste0(second, hyph, third)) %>% stringi::stri_trim_left()

  num_char <- num_char %||% NULL

  return(num_char)

}


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
#'
#' @param ... Character vectors to pass to \code{paste()}.
#' @param sep Separator to use.
#' @param collapse Character to use when \code{paste()} collapses on multiple
#'   vectors.
#'
#' @export


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
