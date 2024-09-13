#' @eval get_description('filter_na')
#' @export
#' @include annotation_source_class.R
filter_na <- function(column_name,
                      mode = "exclude",
                      ...) {
    out <- struct::new_struct("filter_na",
        column_name = column_name,
        mode = mode,
        ...
    )
    return(out)
}

.filter_na <- setClass(
    "filter_na",
    contains = c("model"),
    slots = c(
        column_name = "entity",
        filtered = "entity",
        flags = "entity",
        mode = "entity"
    ),
    prototype = list(
        name = "Filter by missing values",
        description = paste0(
            "Filters annotations where the named column is NA"
        ),
        type = "univariate",
        predicted = "filtered",
        .params = c("column_name", "mode"),
        .outputs = c("filtered", "flags"),
        column_name = entity(
            name = "Column name",
            description = "The column name to use for filtering.",
            type = c("character"),
            value = "V1",
            max_length = 1
        ),
        mode = enum(
            name = "Filter mode",
            description = c(
                "include" = "Rows with NA are kept and all others removed.",
                "exclude" = "Rows with NA are excluded and all other kept."
            ),
            value = "exclude",
            allowed = c("include", "exclude")
        ),
        filtered = entity(
            name = "Filtered annotations",
            description = "annotation_source after filtering.",
            type = "annotation_source",
            max_length = Inf
        ),
        flags = entity(
            name = "Flags",
            description = paste0(
                "A list of flags indicating which annotations were removed."
            ),
            value = data.frame(),
            type = "data.frame",
            max_length = Inf
        )
    )
)


#' @export
setMethod(
    f = "model_apply",
    signature = c("filter_na", "annotation_source"),
    definition = function(M, D) {
        X <- D$data

        if (nrow(X) == 0) {
            # nothing to filter, so return
            M$filtered <- D
            return(M)
        }


        flags <- data.frame(
            na_flag = is.na(X[[M$column_name]]),
            value = X[[M$column_name]]
        )
        colnames(flags)[2] <- M$column_name

        rownames(flags) <- rownames(X)

        M$flags <- flags

        if (M$mode == "exclude") {
            X <- X[!M$flags[, 1], ]
        } else {
            X <- X[M$flags[, 1], ]
        }

        D$data <- X

        M$filtered <- D

        return(M)
    }
)
