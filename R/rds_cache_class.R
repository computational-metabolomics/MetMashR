#' @eval get_description('rds_cache')
#' @export
#' @include rds_database_class.R
#' @family annotation databases
rds_cache <- function(source = character(0),
    data = data.frame(
        .search = character(0)
    ),
    ...) {
    if (nrow(data) == 0 & ncol(data) == 0) {
        data <- data.frame(.search = NA)
    }

    # new object
    out <- struct::new_struct(
        "rds_cache",
        source = source,
        data = data,
        ...
    )
    return(out)
}

.rds_cache <- setClass(
    "rds_cache",
    contains = "rds_database",
    slots = c(
        search = "character"
    ),
    prototype = list(
        name = "rds cache",
        description = paste0(
            "A data.frame stored as an RDS file. Intended to be used ",
            "with `rest_api` objects as mechanism for caching search results. ",
            "The data.frame for an `rds_cache` object must have a column ",
            'named ".search".'
        ),
        type = "rds_cache",
        .writable = TRUE,
        .required = ".search"
    )
)
