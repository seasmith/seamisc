
# facet_filter_count() ----------------------------------------------------
#'
#' Facet data on categorical variable counts.
#' 
#' @param formula Faceting formula; logical conditions on \code{n}, the number
#'   of counts of each grouping in \code{...}.
#' @param ... Grouping variables; these variables will be grouped and then
#'   counted.
facet_filter_count <- function(formula, ...) {
  
  ## Each '...' should go into a separate 'dplyr::group_by()'
  # filter1 <- Master %>%
  #   dplyr::filter(!is.na(birthYear)) %>%  # this is just a filter for example
  #   dplyr::filter(!is.na(debut)) %>%      # this is just a filter for example
  #   dplyr::group_by(birthYear, year(debut)) %>%
  #   dplyr::count()
  
  ## Each 'formula' should go inside 'dplyr::mutate()'
  # filter2 <- dplyr::filter1 %>%
  #   dplyr::mutate(n > 50,
  #          n <= 50 & n >= 11,
  #          n <= 10)
}
