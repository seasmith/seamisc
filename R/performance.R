
# # timeEval() ------------------------------------------------------------
# #
# #' A simple evaluation timer.
# #'
# #' \code{proc.time()} to keep track of all three different time types:
# #' \code{user}, \code{system}, and \code{elapsed}.
# #'
# #' @param x An expression, such as a function, arithmetic equation, etc.
# #' @export
#
# timeEval <- function(x){
#     p1 <- proc.time()
#     eval(x)
#     p2 <- proc.time() - p1
#     print(p2)
# }
