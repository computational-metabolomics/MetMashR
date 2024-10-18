#' @eval get_description('calc_rt_diff')
#' @export
#' @include annotation_table_class.R
calc_rt_diff <- function(obs_rt_column,
    ref_rt_column,
    out_column,
    check_names = "unique",
    ...) {
    out <- struct::new_struct(
        "calc_rt_diff",
        obs_rt_column = obs_rt_column,
        ref_rt_column = ref_rt_column,
        out_column = out_column,
        check_names = check_names,
        ...
    )
    return(out)
}



.calc_rt_diff <- setClass(
    "calc_rt_diff",
    contains = c("model"),
    slots = c(
        obs_rt_column = "entity",
        ref_rt_column = "entity",
        out_column = "entity",
        check_names = "enum",
        updated = "entity"
    ),
    prototype = list(
        name = "Calculate RT difference",
        description = paste0("Calculate RT difference between two RT values"),
        type = "univariate",
        predicted = "updated",
        .params = c(
            "obs_rt_column", "ref_rt_column", "out_column",
            "check_names"
        ),
        .outputs = c("updated"),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The input annotation source with the newly generated ",
                "column."
            ),
            type = "annotation_table"
        ),
        obs_rt_column = entity(
            name = "Observed m/z",
            description = paste0(
                "Column name in annotation table containing the observed ",
                "(measured) RT values."
            ),
            type = "character"
        ),
        ref_rt_column = entity(
            name = "Reference m/z",
            description = paste0(
                "Column name in annotation table containing the reference ",
                "(theoretical) RT values."
            ),
            type = "character"
        ),
        out_column = entity(
            name = "Outout column",
            description = paste0(
                "Column name in annotation table to store the computed RT ",
                "differences."
            ),
            type = "character"
        ),
        check_names = enum(
            name = "Check names",
            description = c(
                "stop" = paste0(
                    "If the output column already exists an error will be ",
                    "thrown."
                ),
                "unique" = paste0(
                    "If the output column already exists a unique column ",
                    "name will be generated."
                ),
                "replace" = paste0(
                    "If the output column already exists it will be replaced."
                )
            ),
            allowed = c("stop", "unique", "replace"),
            value = "unique",
            type = "character"
        )
    )
)


#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("calc_rt_diff", "annotation_table"),
    definition = function(M, D) {
        # calculate RT difference
        rt_diff <- as.numeric(D$data[[M$obs_rt_column]]) -
            as.numeric(D$data[[M$ref_rt_column]])

        # check column names
        colname_check <- M$out_column %in% colnames(D$data)

        # respond as user requested
        if (M$check_names == "stop" & colname_check) {
            stop(
                "There is already a column called ", M$out_column,
                " in the annotation table."
            )
        } else if (M$check_names == "unique" & colname_check) {
            n <- make.names(c(colnames(D$data), M$out_column), unique = TRUE)
            n <- n[length(n)]
            M$out_column <- n
            message("A unique column name was generated from the one provided.")
            D$data[[M$out_column]] <- rt_diff
        } else {
            D$data[[M$out_column]] <- rt_diff
        }

        M$updated <- D

        return(M)
    }
)
