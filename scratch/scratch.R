eg <- function (..., KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE)
{
    nargs <- length(args <- list(...))

    # handle if no args
    if (!nargs)
        return(as.data.frame(list()))
    if (nargs == 1L && is.list(a1 <- args[[1L]]))
        nargs <- length(args <- a1)
    if (nargs == 0L)
        return(as.data.frame(list()))

    # set elements of list equal to nargs
    cargs <- vector("list", nargs)

    iArgs <- seq_len(nargs)
    nmc <- paste0("Var", iArgs)
    nm <- names(args)

    # names
    if (is.null(nm))
        nm <- nmc
    else if (any(ng0 <- nzchar(nm)))
        nmc[ng0] <- nm[ng0]
    names(cargs) <- nmc

    rep.fac <- 1L

    d <- lengths(args)

    if (KEEP.OUT.ATTRS) {
        dn <- vector("list", nargs)
        names(dn) <- nmc
    }

    orep <- prod(d)

    # if one of the args has length zero
    if (orep == 0L) {
        for (i in iArgs) cargs[[i]] <- args[[i]][FALSE]
    }
    # if none of the args has length zero
    else {
        for (i in iArgs) {
            # take out the ith argument
            x <- args[[i]]
            if (KEEP.OUT.ATTRS)
                dn[[i]] <- paste0(nmc[i], "=", if (is.numeric(x))
                    format(x)
                    else x)
            nx <- length(x)
            orep <- orep/nx
            x <- x[rep.int(rep.int(seq_len(nx), rep.int(rep.fac,
                                                        nx)),
                           orep)]
            if (stringsAsFactors && !is.factor(x) && is.character(x))
                x <- factor(x, levels = unique(x))
            cargs[[i]] <- x
            rep.fac <- rep.fac * nx
        }
    }
    if (KEEP.OUT.ATTRS)
        attr(cargs, "out.attrs") <- list(dim = d, dimnames = dn)
    rn <- .set_row_names(as.integer(prod(d)))
    structure(cargs, class = "data.frame", row.names = rn)
}
