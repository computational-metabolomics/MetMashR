#' @eval get_description('sqlite_database')
#' @export
#' @include annotation_database_class.R
#' @family {database}
sqlite_database <- function(
        source,
        table = "annotation_database",
        ...) {
    # new object
    out <- struct::new_struct(
        "sqlite_database",
        source = source,
        table = table,
        ...
    )
    return(out)
}

.sqlite_database <- setClass(
    "sqlite_database",
    contains = "annotation_database",
    slots = c(table = "entity"),
    prototype = list(
        name = "SQLite database",
        description = "A data.frame stored in an SQLite database.",
        type = "database",
        libraries = "RSQLite",
        .params = "table",
        table = entity(
            name = "Table name",
            description = "The name of a table in the SQLite database",
            type = "character"
        ),
        .writable = TRUE
    )
)


#' @export
setMethod(
    f = "read_database",
    signature = c("sqlite_database"), definition = function(obj) {
        # connect to database; create if doesnt exist
        conn <- RSQLite::dbConnect(RSQLite::SQLite(), obj$source)
        
        # read the table
        IN <- RSQLite::dbGetQuery(conn, paste0(
            "SELECT * FROM ", obj$table
        ))
        
        # disconnect
        RSQLite::dbDisconnect(conn)
        
        # return
        return(IN)
    }
)

#' @export
setMethod(
    f = "write_database",
    signature = c("sqlite_database"), definition = function(obj, cache) {
        # connect to database; create if doesnt exist
        conn <- RSQLite::dbConnect(RSQLite::SQLite(), obj$source)
        
        # write df to database
        RSQLite::dbWriteTable(conn, obj$table, cache, overwrite = TRUE)
        
        # disconnect
        RSQLite::dbDisconnect(conn)
        
        # return
        return(invisible(TRUE))
    }
)
