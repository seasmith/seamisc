
# gclass() ----------------------------------------------------------------
#'
#' Guess classes of a file.
#'
#' @description
#' This is a quick-and-dirty method of guessing a file's classes (hence the
#' g in \code{gclass()}).
#'
#' @param x A file to read lines from and guess its columns' classes.
#' @param n The number of lines you want \code{gclass()} to read. Default is 6.
#' @param ... Other parameters to pass to \code{gclass()}. See \code{Details}.
#'
#' @details
#' \code{gclass()} makes an estiamte as to which read function to use. For
#' example, if you pass the parameter \code{widths} then \code{gclass} assumes
#' you are trying to read from a fixed-width file and so uses \code{read.fwf} to
#' read the number of lines specified by the parameter \code{n}.
#' @export


gclass <- function(x, n = 6L, ...){
  classes <- readLines(file, n = n) %>%
    textConnection() %>%
    read.csv(stringsAsFactors = FALSE) %>%
    sapply(class)
}
