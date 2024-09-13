#' @eval get_description('database_lookup')
#' @export
#' @include annotation_source_class.R
database_lookup <- function(query_column,
                            database_column,
                            database,
                            include = NULL,
                            suffix = NULL,
                            not_found = NA,
                            ...) {
    out <- struct::new_struct(
        "database_lookup",
        query_column = query_column,
        database_column = database_column,
        database = database,
        include = include,
        suffix = suffix,
        not_found = not_found,
        ...
    )
    return(out)
}



.database_lookup <- setClass(
    "database_lookup",
    contains = c("model"),
    slots = c(
        query_column = "entity",
        database_column = "entity",
        database = "entity",
        include = "entity",
        updated = "entity",
        suffix = "entity",
        not_found = "entity"
    ),
    prototype = list(
        name = "ID lookup by database",
        description = paste0(
            "Search a database (data.frame) for annotation matches based on ",
            "values in a specified column."
        ),
        type = "univariate",
        predicted = "updated",
        libraries = "dplyr",
        .params = c(
            "query_column", "database_column", "database", "include",
            "suffix", "not_found"
        ),
        .outputs = c("updated"),
        query_column = entity(
            name = "Annotions column name",
            description = paste0(
                "The annotation table column name to use as ",
                'the reference for searching the database e.g. "HMBD_ID"'
            ),
            type = c("character"),
            value = "V1",
            max_length = 1
        ),
        database_column = entity(
            name = "Database column name",
            description = paste0(
                "The database column to search for matches ",
                "to the values in annoation_column."
            ),
            type = c("character"),
            value = "",
            max_length = 1
        ),
        database = entity(
            name = "Database",
            description = paste0(
                "A database to be searched. Can be a `data.frame` or a ",
                "`annotation_database` object."
            ),
            type = c("data.frame", "annotation_database")
        ),
        include = entity(
            name = "Include columns",
            description = paste0(
                "The name of the database columns to be ",
                "added to the annotations. If NULL, all columns are ",
                "retained."
            ),
            type = c("character", "NULL"),
            max_length = Inf
        ),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The input annotation_source is updated with matching ",
                "columns from the database."
            ),
            type = "annotation_source"
        ),
        suffix = entity(
            name = "Column name suffix",
            description = paste0(
                "A string appended to the column names from the database. ",
                "Used to distinguish columns from different databases with ",
                "identical column names.",
                "If suffix = NULL then the column names are not changed."
            ),
            type = c("character", "NULL"),
            max_length = 1
        ),
        not_found = entity(
            name = "Not found",
            description = "The returned value when there are no matches.",
            type = c("character", "numeric", "logical", "NULL"),
            max_length = 1
        )
    )
)


#' @export
setMethod(
    f = "model_apply",
    signature = c("database_lookup", "annotation_source"),
    definition = function(M, D) {
        # if a annotation_database, read it in
        stdb <- FALSE
        if (is(M$database, "annotation_database")) {
            origdb <- M$database
            M$database <- read_database(M$database)
        }

        if (is.null(M$include)) {
            M$include <- colnames(M$database)
        }
        X <- D$data

        # match class to db
        X[[M$query_column]] <- as(
            X[[M$query_column]],
            class(M$database[[M$database_column]])
        )

        # for each annotation
        OUT <- apply(X, 1, function(x) {
            # search for database rows that match the annotation column
            w <- which(M$database[[M$database_column]] ==
                x[[M$query_column]])

            if (length(w) == 0) {
                # if no hits in db then return no_match
                found <- M$database[1, , drop = FALSE]
                found[1, ] <- M$not_found
                found[[M$database_column]] <- x[[M$query_column]]
            } else {
                found <- M$database[w, , drop = FALSE]
            }
            return(found)
        })
        OUT <- do.call(rbind, OUT)

        # include only requested
        OUT <- OUT[, unique(c(M$database_column, M$include)), drop = FALSE]

        # remove duplicates
        OUT <- unique(OUT)

        # add suffixs
        if (!is.null(M$suffix)) {
            colnames(OUT) <- paste0(colnames(OUT), M$suffix)
            db_column <- paste0(M$database_column, M$suffix)
        } else {
            db_column <- M$database_column
        }

        # match by provided columns
        by <- db_column
        names(by) <- M$query_column

        # merge with original table
        merged <- dplyr::left_join(X, OUT, by = by)

        D$data <- merged

        M$updated <- D

        # reset db so we dont store huge tables multiple times
        if (stdb) {
            M$database <- orig_db
        }

        return(M)
    }
)
