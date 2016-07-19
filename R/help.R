
# magrittr::`%>%` ---------------------------------------------------------
#' This is an alias of magrittr::`%>%` for convenient calling

`%>%` <- magrittr::`%>%`


# magrittr::`%<>%` --------------------------------------------------------
#' This is an alias of magrittr::`%<>%` for convenient calling

`%<>%` <- magrittr::`%<>%`



# showPackageContents() ---------------------------------------------------
#
# Source found in Winston Chang's "Cookbook for R" at the following link:
# http://www.cookbook-r.com/Scripts_and_functions/Getting_a_list_of_functions_and_objects_in_a_package/

showPackageContents <- function (packageName) {

    # Get a list of things contained in a particular package
    funlist <- objects(packageName)

    # Remove things that don't start with a letter
    idx <- grep('^[a-zA-Z][a-zA-Z0-9._]*', funlist)
    funlist <- funlist[idx]

    # Remove things that contain arrow <-
    idx <- grep('<-', funlist)
    if (length(idx)!=0)
        funlist <- funlist[-idx]

    # Make a data frame to keep track of status
    objectlist <- data.frame(name=funlist,
                             primitive=FALSE,
                             func=FALSE,
                             object=FALSE,
                             constant=FALSE,
                             stringsAsFactors=F)

    for (i in 1:nrow(objectlist)) {
        fname <- objectlist$name[i]
        if (exists(fname)) {
            obj <- get(fname)
            if (is.primitive(obj)) {
                objectlist$primitive[i] <- TRUE
            }
            if (is.function(obj)) {
                objectlist$func[i] <- TRUE
            }
            if (is.object(obj)) {
                objectlist$object[i] <- TRUE
            }

            # I think these are generally constants
            if (is.vector(obj)) {
                objectlist$constant[i] <- TRUE
            }


        }
    }

    cat(packageName)

    cat("\n================================================\n")
    cat("Primitive functions: \n")
    cat(objectlist$name[objectlist$primitive])
    cat("\n")

    cat("\n================================================\n")
    cat("Non-primitive functions: \n")
    cat(objectlist$name[objectlist$func  &  !objectlist$primitive])
    cat("\n")

    cat("\n================================================\n")
    cat("Constants: \n")
    cat(objectlist$name[objectlist$constant])
    cat("\n")

    cat("\n================================================\n")
    cat("Objects: \n")
    cat(objectlist$name[objectlist$object])
    cat("\n")
}
