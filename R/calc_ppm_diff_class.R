#' @eval get_description('calc_ppm_diff')
#' @export
#' @include annotation_table_class.R
calc_ppm_diff <- function(
    obs_mz_column,
    ref_mz_column,
    out_column,
    check_names = "unique",
    ...) {
    out <- struct::new_struct("calc_ppm_diff",
        obs_mz_column = obs_mz_column,
        ref_mz_column = ref_mz_column,
        out_column = out_column,
        check_names = check_names,
        ...
    )
    return(out)
}



.calc_ppm_diff <- setClass(
    "calc_ppm_diff",
    contains = c("model"),
    slots = c(
        obs_mz_column = "entity",
        ref_mz_column = "entity",
        out_column = "entity",
        check_names = "enum",
        updated = "entity"
    ),
    prototype = list(
        name = "Calculate ppm difference",
        description = paste0(
            "Calculate ppm difference between two columns in an ",
            "[`annotation_table`]. e.g. for comparing observed m/z to ",
            "theortical ones."
        ),
        type = "comparison",
        predicted = "updated",
        .params = c(
            "obs_mz_column", "ref_mz_column", "out_column",
            "check_names"
        ),
        .outputs = c("updated"),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The input annotation source with the computed ppm diffences ",
                "in a new column."
            ),
            type = "annotation_table"
        ),
        obs_mz_column = entity(
            name = "Observed m/z",
            description = paste0(
                "Column name in annotation_table containing the observed ",
                "m/z values."
            ),
            type = "character"
        ),
        ref_mz_column = entity(
            name = "Reference m/z",
            description = "Column name in annotation table containing the ",
            "reference (theoretical) m/z values.",
            type = "character"
        ),
        out_column = entity(
            name = "Outout column",
            description =
                paste0(
                    "Column name in annotation table to store the computed ",
                    "ppm differences."
                ),
            type = "character"
        ),
        check_names = enum(
            name = "Check names",
            description = c(
                "stop" = paste0(
                    "If the output column already exists an error will ",
                    "be thrown."
                ),
                "unique" = paste0(
                    "If the output column already exists a ",
                    "unique column name will be generated."
                ),
                "replace" = paste0(
                    "If the output column already exists it will be ",
                    "replaced."
                )
            ),
            allowed = c("stop", "unique", "replace"),
            value = "unique",
            type = "character"
        )
    )
)


#' @export
setMethod(
    f = "model_apply",
    signature = c("calc_ppm_diff", "annotation_table"),
    definition = function(M, D) {
        OUT <- D$data

        check <- M$obs_mz_column %in% colnames(OUT)
        if (!check) {
            stop("Observed mz column is not present in annotation table")
        }
        check <- M$ref_mz_column %in% colnames(OUT)
        if (!check) {
            stop("Reference mz column is not present in annotation table")
        }

        # calculate ppm difference
        diff <- as.numeric(OUT[[M$obs_mz_column]]) -
            as.numeric(OUT[[M$ref_mz_column]])
        ppm_diff <- 1e6 * diff / as.numeric(OUT[[M$ref_mz_column]])

        # check column names
        colname_check <- M$out_column %in% colnames(OUT)


        # respond as user requested
        if (M$check_names == "stop" & colname_check) {
            stop(
                "There is already a column called ", M$out_column,
                " in the annotation table."
            )
        } else if (M$check_names == "unique" & colname_check) {
            n <- make.names(c(colnames(OUT), M$out_column), unique = TRUE)
            n <- n[length(n)]
            M$out_column <- n
            message("A unique column name was generated from the one provided.")
            OUT[[M$out_column]] <- ppm_diff
        } else {
            OUT[[M$out_column]] <- ppm_diff
        }

        D$data <- OUT
        M$updated <- D

        return(M)
    }
)
