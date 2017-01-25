# mon_year() --------------------------------------------------------------
#
#' Convert dates string elements to month and year (mm/yyyy).
#' 
#' @param date Date vector.
#' @param sep Month-year separator. Default is \code{"/"}.

mon_year <- function(date, sep = "/") {
  paste0(lubridate::month(date), sep, lubridate::year(date))
}




# seq_fill() --------------------------------------------------------------
#
#' Fill in missing sequences
#' 
#' @param x A sequence of numbers.
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