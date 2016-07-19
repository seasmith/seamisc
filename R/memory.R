# First two functions are taken from the StackOverflow answer found
# at the following link:
# http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session

# ls.objects() ------------------------------------------------------------


ls.objects <- function(pos = 1, pattern, order.by,
                        decreasing = FALSE, head = FALSE, n = 5){
    napply <- function(names, fun) sapply(names, function(x){
        fun(get(x, pos = pos))
    })
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)
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


# ls.os() ------------------------------------------------------------------


# Shorthand form of .ls.objects().
lsos <- function(..., n = 10) {
    .ls.objects(..., order.by = "Size",
                decreasing = TRUE, head = TRUE, n = n)
}



# ls.mem() ----------------------------------------------------------------

# Find the total memory of all objects
ls.mem <- function(pos = 1, all.names = TRUE, unit = "MB"){
    # get object sizes and sum all sizes to find total
    obj <- ls(pos = pos, all.names = all.names)
    obj.mem <- sapply(obj, function(x){
        object.size(eval(as.name(x)))
    })
    obj.mem.sum <- sum(obj.mem)

    # decide which divisor to use based on the unit argument
    unit.name <- c("B", "KB", "MB", "GB")
    arg <- match.arg(unit, unit.name)
    which(unit.name == arg)
    unit.div <- c(1, 1024, 1048576, 1073741824 )

    obj.mem.sum/unit.div[which(unit.name == arg)]
}
