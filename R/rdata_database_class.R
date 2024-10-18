#' @eval get_description('rdata_database')
#' @export
#' @include annotation_database_class.R
#' @family annotation databases
rdata_database <- function(
        source = character(0),
        variable_name,
        ...) {
    # new object
    out <- struct::new_struct(
        "rdata_database",
        source = source,
        variable_name = variable_name,
        ...
    )
    return(out)
}

.rdata_database <- setClass(
    "rdata_database",
    contains = "annotation_database",
    slots = c(
        variable_name = "entity"
    ),
    prototype = list(
        name = "rdata database",
        description = "A data.frame stored as an RData file.",
        type = "rdata_database",
        .writable = TRUE,
        .params=c('variable_name'),
        variable_name = entity(
            name = "Variable name",
            description = paste0(
                "The name of the data.frame in the imported workspace to ",
                "use as the data.frame for this source. A function can be ",
                "provided to e.g. extract a data.frame from a list in the ",
                "imported environment."
            ),
            type = c("character", "function"),
            value = 'a data frame'
        )
    )
)

#' @export
#' @rdname read_database
setMethod(
    f = "read_database",
    signature = c("rdata_database"), definition = function(obj) {
        # new environment
        IN <- new.env()
        
        # import
        load(obj$source, envir = IN)
        
        if (is.character(obj$variable_name)) {
            OUT <- IN[[obj$variable_name]]
        } else { # assume function
            OUT <- obj$variable_name(IN)
        }
        
        # return
        return(OUT)
    }
)


#' @export
#' @rdname is_writable
setMethod(
    f = "is_writable",
    signature = c("rdata_database"),
    definition = function(obj) {
        return(obj@.writable & file.access(obj$source, mode = 2))
    }
)
