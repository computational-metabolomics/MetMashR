#' @eval get_description('AnnotationDb_database')
#' @export
#' @include annotation_database_class.R
#' @family {annotation databases}
#' @seealso [AnnotationDbi::AnnotationDb]
AnnotationDb_database <- function(source,
                                  table,
                                  ...) {
    # new object
    out <- struct::new_struct(
        "AnnotationDb_database",
        source = source,
        table = table,
        ...
    )
    return(out)
}

.AnnotationDb_database <- setClass(
    "AnnotationDb_database",
    contains = "annotation_database",
    slots = c(
        table = "entity"
    ),
    prototype = list(
        name = "AnnotationDb database",
        description = paste0(
            "Retrieve a table from an AnnotationDb package."
        ),
        type = "AnnotationDb_database",
        .writable = FALSE,
        libraries = "AnnotationDbi",
        table = entity(
            name = "Table name",
            description = paste0(
                "The name of a table to import from the specified source ",
                "AnnotationDb package."
            ),
            type = "character",
            max_length = 1
        ),
        source = entity(
            name = "AnnotationDb package name",
            description = paste0(
                "The name of an AnnotationDb package to import the specified ",
                "table from. Note the package should already be installed."
            ),
            type = "character",
            max_length = 1
        ),
        .params = "table"
    )
)

#' @export
setMethod(
    f = "read_database",
    signature = c("AnnotationDb_database"), definition = function(obj) {
        # get resource
        IN <- get(obj$table, envir = loadNamespace(obj$source))
        IN <- as.list(IN)
        IN <- stack(IN)
        colnames(IN)[1] <- obj$source
        # return
        return(IN[, c(2, 1)])
    }
)
