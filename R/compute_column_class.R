#' @eval get_description('compute_column')
#' @export
#' @include annotation_source_class.R
compute_column <- function(input_columns,
    output_column,
    fcn,
    ...) {
    out <- struct::new_struct(
        "compute_column",
        input_columns = input_columns,
        output_column = output_column,
        fcn = fcn,
        ...
    )

    return(out)
}


.compute_column <- setClass(
    "compute_column",
    contains = c("model"),
    slots = c(
        updated = "entity",
        input_columns = "entity",
        output_column = "entity",
        fcn = "entity"
    ),
    prototype = list(
        name = "Compute a column",
        description = paste0(
            "Compute values for a new column based on an input column."
        ),
        type = "pivot",
        predicted = "updated",
        .params = c("input_columns", "output_column", "fcn"),
        .outputs = c("updated"),
        libraries = "dplyr",
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The updated annotations as an `annotation_source` object"
            ),
            type = "annotation_source"
        ),
        input_columns = entity(
            name = "Input column",
            description = paste0(
                "The name of a column in the input table used to compute a ",
                "new column."
            ),
            type = "character"
        ),
        output_column = entity(
            name = "Output column",
            description = paste0(
                "The name of the newply computed column."
            ),
            type = "character",
            max_length = 1
        ),
        fcn = entity(
            name = "Function",
            description = paste0(
                "The function used to compute the values for the new column."
            ),
            type = "function"
        )
    )
)


#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("compute_column", "annotation_source"),
    definition = function(M, D) {
        new_col <- M$fcn(D$data[, M$input_columns, drop = FALSE])
        D$data[[M$output_column]] <- new_col

        M$updated <- D
        return(M)
    }
)
