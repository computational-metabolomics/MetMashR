#' @eval get_description('AnnotationDb_select')
#' @export
#' @include annotation_source_class.R
#' @seealso [dplyr::left_join()]
#' @seealso [AnnotationDbi::select()]
AnnotationDb_select <- function(
        database,
        key_column,
        key_type,
        database_columns,
        drop_na = TRUE,
        ...) {
    out <- struct::new_struct(
        "AnnotationDb_select",
        database = database,
        key_column = key_column,
        key_type = key_type,
        database_columns = database_columns,
        drop_na = drop_na,
        ...
    )
    return(out)
}


.AnnotationDb_select <- setClass(
    "AnnotationDb_select",
    contains = c("model"),
    slots = c(
        updated = "entity",
        database = "entity",
        key_column = "entity",
        key_type = "entity",
        database_columns = "entity",
        drop_na = "entity"
    ),
    prototype = list(
        name = "Select columns from AnnotationDb database",
        description = paste0(
            "A wrapper around `[annotationDbi::select()]` that can be used to ",
            "import columns from the database where the keys are provided by ",
            "a column in the annotation table."
        ),
        type = "AnnotationDb_select",
        predicted = "updated",
        .params = c(
            "database", "key_column", "key_type", "database_columns",
            "drop_na"
        ),
        .outputs = c("updated"),
        libraries = c("dplyr", "AnnotationDbi"),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The updated annotations as an `annotation_source` object"
            ),
            type = "annotation_source"
        ),
        database = entity(
            name = "AnnotationDbi database",
            description = paste0(
                "The name of the AnnotationDbi package/object to ",
                "import."
            ),
            value = "",
            type = "character",
            max_length = 1
        ),
        key_column = entity(
            name = "Key column name",
            description = paste0(
                "The name of a column in the annotation table containing key ",
                "values used to extract records from the AnnotationDbi ",
                "database."
            ),
            value = "",
            type = "character",
            max_length = 1
        ),
        key_type = entity(
            name = "Key type",
            description = paste0(
                "The name of a column in the AnnoationDb database ",
                "searched for matches to the key values."
            ),
            value = "",
            type = "character",
            max_length = 1
        ),
        database_columns = entity(
            name = "Database columns",
            description = paste0(
                "The name of columns to import from the AnnoationDb database. ",
                "Special case `.all` can be used to get all columns."
            ),
            value = ".all",
            type = "character"
        ),
        drop_na = entity(
            name = "Drop NA",
            description = c(
                "TRUE" = paste0(
                    "Remove rows where all columns of the returned ",
                    "database are NA."
                ),
                "FALSE" = paste0(
                    "Keep rows where all columns of the returned ",
                    "database are NA."
                )
            ),
            value = TRUE,
            type = "logical",
            max_length = 1
        )
    )
)



#' @export
setMethod(
    f = "model_apply",
    signature = c("AnnotationDb_select", "annotation_source"),
    definition = function(M, D) {
        # get db
        db <- do.call(`::`, list(M$database, M$database))
        
        # prepare from:to for left join
        by <- M$key_type
        names(by) <- M$key_column
        
        # columns
        if (any(M$database_columns == ".all")) {
            M$database_columns <- AnnotationDbi::columns(db)
        }
        
        # select
        db <- AnnotationDbi::select(
            x = db,
            keys = as.character(D$data[[M$key_column]]),
            columns = M$database_columns,
            keytype = M$key_type
        )
        
        # remove NA
        if (M$drop_na) {
            na <- apply(db, 1, function(x) {
                any(is.na(x))
            })
            db <- db[!na, ]
        }
        
        # unique rows
        db <- unique(db)
        
        # add the columns
        M2 <- add_columns(
            new_columns = db,
            by = by
        )
        M2 <- model_apply(M2, D)
        
        # assign to object
        M$updated <- predicted(M2)
        
        return(M)
    }
)
