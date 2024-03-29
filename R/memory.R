# First two functions are taken from the StackOverflow answer found
# at the following link:
# http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session

# ls.objects() ------------------------------------------------------------
#
#' List objects in the current work space (global environment) and their
#' respective size in bytes - taken from Dirk Eddelbuettel's response at
#' http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session.
#'
#' @param pos Position in environment to search.
#' @param pattern Search pattern for the environment (passed to \code{ls()}).
#' @param order.by Column to order results by.
#' @param decreasing Logical indicating whether to sort results in decreasing
#'   order.
#' @param head Logical indicating whether the results should be truncated by
#'   \code{head()}.
#' @param n Number to truncate results if \code{head = TRUE}.
#'
#' @export

ls.objects <- function(pos = 1, pattern, order.by = "Size",
                        decreasing = TRUE, head = FALSE, n = 5){
    napply <- function(names, fun) sapply(names, function(x){
        fun(get(x, pos = pos))
    })
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, utils::object.size)
    obj.dim <- t(napply(names, function(x)
        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.dim)
    names(out) <- c("Type", "Size", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}


# ls.obs() -----------------------------------------------------------------
#' Shorthand form of ls.objects().
#'
#' @param ... Additional argument to pass to \code{ls.objects()}.
#' @param n Number to pass to \code{head()}.
#'
#' @export

ls.obs <- function(..., n = 10) {
    ls.objects(..., order.by = "Size",
                decreasing = TRUE, head = TRUE, n = n)
}



# ls.mem() ----------------------------------------------------------------
#' Find the total memory of all objects
#'
#' @param pos Integer indicating position within the environment hierarchy to
#'   search within. 1 = Global.
#' @param all.names = Logical indicating whether to include names that begin with
#'   ".".
#' @param unit Character indicating the units to return for size of objects.
#'
#' @export

ls.mem <- function(pos = 1, all.names = TRUE, unit = "MB"){
    # get object sizes and sum all sizes to find total
    obj <- ls(pos = pos, all.names = all.names)
    obj.mem <- sapply(obj, function(x){
        utils::object.size(eval(as.name(x)))
    })
    obj.mem.sum <- sum(obj.mem)

    # decide which divisor to use based on the unit argument
    unit.name <- c("B", "KB", "MB", "GB")
    arg <- match.arg(unit, unit.name)
    which(unit.name == arg)
    unit.div <- c(1, 1024, 1048576, 1073741824 )

    obj.mem.sum/unit.div[which(unit.name == arg)]
}


# ls.summary() ------------------------------------------------------------
#' Summary statistics of objects in workspace by Type.
#'
#' @param size.all Character vector specifying the unit to display
#'   \code{Size_All}. Can be either of \code{"B", "KB", "MB", "GB"}.
#' @param plot Logical indicating whether to output a summary plot.
#' @param order.by Character vector of length one (1) indicating the column to
#'   order by.
#' @export

ls.summary <- function(size.all = "KB", plot = TRUE, order.by = "Size_All") {
  summ <- ls.objects() %>%
  dplyr::group_by(Type) %>%
  dplyr::summarize(Count    = n(),
                   Size_All = sum(Size),
                   Size_Avg = mean(Size),
                   Rows     = sum(Rows),
                   Columns  = sum(Columns))

  resize <- dplyr::case_when(
    size.all == "B"  ~ 10^0,
    size.all == "KB" ~ 10^3,
    size.all == "MB" ~ 10^6,
    size.all == "GB" ~ 10^9,
    size.all == "TB" ~ 10^12
  )

  df.ls <- summ %>%
    dplyr::arrange(desc(Size_All)) %>%
    dplyr::mutate(Size_All = Size_All / resize,
                  Size_Avg = Size_Avg / resize) %>%
    dplyr::select(-Rows, -Columns) %>%
    tidyr::gather(-Type, key = "key", value = "value")

  summ <- summ %>%
    dplyr::arrange(desc(.[[order.by]])) %>%
    dplyr::mutate(Size_All = Size_All / resize,
                  Size_Avg = Size_Avg / resize)

  if (plot) {

      factor.order <- df.ls %>%
        dplyr::filter(key == "Size_All") %>%
        dplyr::select(Type) %>%
        unlist() %>%
        as.character()

      df.ls$Type <- factor(df.ls$Type, levels = factor.order)

    df.ls <- df.ls %>%
      dplyr::ungroup() %>%
      dplyr::arrange(key, value) %>%
      dplyr::mutate(.r = row_number())

    p <- df.ls %>%
      ggplot2::ggplot(ggplot2::aes(.r, value, fill = Type)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::facet_wrap(~key, scales = "free") +
      ggplot2::scale_x_continuous(breaks = df.ls$.r,
                                  labels = df.ls$Type) +
      ggplot2::coord_flip() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

  } else {
    return(summ)
  }
  print(p)
  return(summ)
}
