# mon_year() --------------------------------------------------------------
#
#' Convert dates string elements to month and year (mm/yyyy).
#'
#' @param date Date vector.
#' @param sep Month-year separator. Default is \code{"/"}.
#' @export

mon_year <- function(date, sep = "/") {
  paste0(lubridate::month(date), sep, lubridate::year(date))
}




# seq_fill() --------------------------------------------------------------
#
#' Fill in missing sequences
#'
#' @param x A sequence of numbers.
#' @param df A dataframe containing a column of type mm/yyyy, in some format
#' @param col The quoted name of the column in \code{df} containing the mm/yyyy
#'   column
#' @param seq.type The type of sequence: \code{"numeric"} (default) or
#'   \code{"month"}.
#' @param return.list Logical. Return a list (default) or a single numeric
#'   vector.
#' @export

seq_fill <- function(x, seq.type = "month", return.list = TRUE) {
  cs <- cumsum(c(1, diff(x) != 1))


  # out <- numeric(length(x))
  out <- list()
  for(i in seq(x)[-length(x)]) {

    if(cs[i] == cs[i + 1]) {

      out[[i]] <- x[i]

    } else {

      out[[i]] <- seq(x[i], x[i + 1] - 1)

    }
  }

  out[[length(x)]] <- x[length(x)]

  if(seq.type == "numeric") {

    return(out)

  } else if (seq.type == "month") {

    wrong <- out %>% vapply(function(x) {

      cs <- cumsum(c(1, diff(x) != 1))
      !any(duplicated(cs)) & length(cs) > 1

    }, logical(1))

    out[wrong] %<>% sapply(function(x){
      seq_1 <- seq(x[1], 12)
      seq_2 <- if(x[length(x)] != 0) seq(1, x[length(x)]) else NULL
      c(seq_1, seq_2)
    })
    return(out)
  }
  return(out)
}

#' @rdname seq_fill
#' @export

seq_fill_date <- function(x) {

  # Split x by "/" or "-" regex
  regex <- x %>% stringi::stri_extract(regex = "[^0-9]") %>% unique()
  x     <- x %>% stringi::stri_split(regex = regex)

  # Extract month and year
  month <- x %>% sapply(`[`, 1) %>% as.numeric()
  year <- x %>% sapply(`[`, 2) %>% as.numeric()

  # Fill in missing month and year sequences
  month <- month %>% seq_fill() %>% unlist()
  index  <- month %>% {cumsum(c(1, diff(.) != 1))}
  year <- year[index]

  # Return x into its original format using regex
  x <- paste0(month, regex, year)
  return(x)
}

#' @rdname seq_fill
#' @export

seq_fill_date_df <- function(df, col) {

    other_cols <- df[, -grep(col, names(df))]
    x <- df[, col][[1]]

    regex <- x %>% stringi::stri_extract(regex = "[^0-9]") %>% unique()
    x     <- x %>% stringi::stri_split(regex = regex)

    month <- x %>% sapply(`[`, 1) %>% as.numeric() %>% seq_fill()
    index <- month %>% unlist() %>% {cumsum(c(1, diff(.) != 1))}
    year  <- x %>% sapply(`[`, 2) %>% as.numeric() %>% unique() %>% `[`(index)


    na_fills <- month %>%
        lengths() %>%
        -1 %>%
        sapply(function(x) rep(NA, x))

    other_cols <- other_cols %>% lapply(function(x) {
        Map(c, as.list(x), na_fills) %>%
            unlist()
    })

    new_col <- paste0(unlist(month), regex, year)
    new_df <- data.frame(list(new_col), other_cols)
    names(new_df) <- names(df)

    return(tibble::as_tibble(new_df))
}
