#' @eval get_description('split_records')
#' @export
#' @include annotation_source_class.R
split_records <- function(column_name,
                          separator,
                          clean = TRUE,
                          ...) {
    out <- struct::new_struct(
        "split_records",
        column_name = column_name,
        separator = separator,
        clean = clean,
        ...
    )
    return(out)
}


.split_records <- setClass(
    "split_records",
    contains = c("model"),
    slots = c(
        updated = "entity",
        column_name = "entity",
        separator = "entity",
        clean = "entity"
    ),
    prototype = list(
        name = "Expand records",
        description = paste0(
            "Expand single records into multiple records by splitting strings ",
            "in a named column at the chosen separator. For example, if a ",
            'for a record the column `synonyms = c("glucose,dextrose")` then ',
            "by splitting at the comma results in two records, one for ",
            "glucose and one for dextrose with identical values (apart from ",
            "the column being split). The original record is removed."
        ),
        type = "split_records",
        predicted = "updated",
        .params = c("column_name", "separator", "clean"),
        .outputs = c("updated"),
        libraries = "tidytext",
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The updated annotations as an `annotation_source` object"
            ),
            type = "annotation_source"
        ),
        column_name = entity(
            name = "Column name",
            description = paste0(
                "The column name of the `annotation_source` to split into",
                "multiple records"
            ),
            type = "character"
        ),
        separator = entity(
            name = "Separator",
            description = paste0(
                "The substring used to split the values in column_name into ",
                "multiple records"
            ),
            type = "character",
            value = ","
        ),
        clean = entity(
            name = "Clean columns",
            description = paste0(
                "Remove the original column. If FALSE the original column ",
                "will be retained in the final output with .original appended ",
                "to the column name."
            ),
            value = TRUE,
            type = "logical"
        )
    )
)


#' @export
setMethod(
    f = "model_apply",
    signature = c("split_records", "annotation_source"),
    definition = function(M, D) {
        D$data$.original <- D$data[[M$column_name]]

        D$data <- tidytext::unnest_tokens(
            tbl = D$data,
            output = ".output",
            input = ".original",
            token = "regex",
            pattern = M$separator,
            to_lower = FALSE
        )

        # fix column names
        D$data[[M$column_name]] <- D$data[[".output"]]
        D$data$.output <- NULL

        if (M$clean) {
            D$data$.original <- NULL
        }

        M$updated <- D

        return(M)
    }
)
