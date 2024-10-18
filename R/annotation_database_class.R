#' @eval get_description("annotation_database")
#' @include annotation_source_class.R
#' @family annotation databases
#' @family annotation sources
#' @export
annotation_database <- function(data = data.frame(),
    tag = "",
    ...) {
    # new object
    out <- new_struct(
        "annotation_database",
        data = data,
        tag = tag,
        ...
    )

    return(out)
}

.annotation_database <- setClass(
    "annotation_database",
    contains = c("annotation_source"),
    slots = c(
        .writable = "logical"
    ),
    prototype = list(
        name = "An annotation database",
        description = paste0(
            "An `annotation_database` is an [`annotation_source()`] where the ",
            "imported data.frame contains meta data for annotations. For ",
            "example it might be a table of molecular identifiers, associated ",
            "pathways etc."
        ),
        .writable = FALSE
    )
)


#' @export
#' @rdname is_writable
setMethod(
    f = "is_writable",
    signature = c("annotation_database"),
    definition = function(obj) {
        return(obj@.writable)
    }
)

#' @export
#' @rdname read_source
setMethod(
    f = "read_source",
    signature = c("annotation_database"),
    definition = function(obj) {
        obj$data <- read_database(obj)
        return(obj)
    }
)


#' @export
#' @rdname write_database
#' @param df (data.frame) the data.frame to store in the database.
setMethod(
    f = "write_database",
    signature = c("annotation_database"), definition = function(obj, df) {
        msg <- NULL

        check <- obj@.writable
        if (!check) {
            msg <- c(msg, paste0(
                "The ", class(obj), ' does not have a
                      "write_database" method defined.'
            ))
        }

        # return
        return(invisible(TRUE))
    }
)

#' @export
#' @rdname read_database
setMethod(
    f = "read_database",
    signature = c("annotation_database"),
    definition = function(obj) {
        stop(
            "The ", class(obj), " does not have a read_database method ",
            "defined."
        )
        return(obj)
    }
)
