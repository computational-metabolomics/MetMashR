#' @eval get_description('rds_database')
#' @export
#' @include annotation_database_class.R
#' @family annotation databases
rds_database <- function(
        source = character(0),
        ...) {
    # new object
    out <- struct::new_struct(
        "rds_database",
        source = source,
        ...
    )
    return(out)
}

.rds_database <- setClass(
    "rds_database",
    contains = "annotation_database",
    prototype = list(
        name = "rds database",
        description = "A data.frame stored as an RDS file.",
        type = "rds_database",
        .writable = TRUE
    )
)

#' @export
#' @rdname read_database
setMethod(
    f = "read_database",
    signature = c("rds_database"), definition = function(obj) {
        # if file doesnt exist, create it
        check <- file.exists(obj$source)
        if (!check) {
            saveRDS(obj$data, file = obj$source)
        }
        
        # read the file
        IN <- readRDS(obj$source)
        
        # return
        return(IN)
    }
)

#' @export
#' @rdname write_database
setMethod(
    f = "write_database",
    signature = c("rds_database"), definition = function(obj, df) {
        check <- is_writable(obj)
        if (!check) {
            stop(
                "This ", class(obj), " is not writable. Check file ",
                "permissions."
            )
        }
        
        # write df to file
        saveRDS(object = df, file = obj$source)
        
        # return
        return(invisible(TRUE))
    }
)
