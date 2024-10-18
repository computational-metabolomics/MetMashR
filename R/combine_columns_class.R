#' @eval get_description('combine_columns')
#' @export
#' @include annotation_source_class.R
combine_columns <- function(column_names,
    separator = "_",
    prefix = NULL,
    suffix = NULL,
    output_column = "combined",
    clean = TRUE,
    ...) {
    out <- struct::new_struct(
        "combine_columns",
        column_names = column_names,
        separator = separator,
        prefix = prefix,
        suffix = suffix,
        output_column = output_column,
        clean = clean,
        ...
    )
    return(out)
}

.combine_columns <- setClass(
    "combine_columns",
    contains = "model",
    slots = c(
        column_names = "entity",
        separator = "entity",
        prefix = "entity",
        suffix = "entity",
        output_column = "entity",
        updated = "entity",
        clean = "entity"
    ),
    prototype = list(
        name = "Combine columns",
        description = paste0(
            "A wrapper for [`paste()`] and [`interaction()`]. Combines the ",
            "values in multiple columns row-wise."
        ),
        type = "processing",
        predicted = "updated",
        .params = c(
            "column_names", "separator", "output_column", "clean",
            "prefix", "suffix"
        ),
        .outputs = c("updated"),
        column_names = entity(
            name = "Column names",
            description = paste0(
                "The column name(s) in the annotation_source to combine."
            ),
            type = c("character"),
            value = "V1",
            max_length = Inf
        ),
        separator = entity(
            name = "Separator",
            description = paste0(
                "A string placed in between the two being joined."
            ),
            type = "character",
            max_length = 1,
            value = "_"
        ),
        prefix = entity(
            name = "Prefix",
            description = paste0(
                "A string placed at the start of the combined strings"
            ),
            type = c("character", "NULL"),
            max_length = 1,
            value = NULL
        ),
        suffix = entity(
            name = "Suffix",
            description = paste0(
                "A string placed at the end of the combined strings"
            ),
            type = c("character", "NULL"),
            max_length = 1,
            value = NULL
        ),
        output_column = entity(
            name = "Column name",
            description = paste0(
                "The name of a column to store the combined values in."
            ),
            value = "combined",
            type = "character",
            max_length = 1
        ),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The annotation_source after combining the columns."
            ),
            type = "annotation_source",
            max_length = Inf
        ),
        clean = entity(
            name = "Clean old columns",
            description = c(
                "TRUE" = "The named columns are removed after being combined.",
                "FALSE" = "The named columns are retained after being combined."
            ),
            value = TRUE,
            type = "logical",
            max_length = 1
        )
    )
)


#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("combine_columns", "annotation_source"),
    definition = function(M, D) {
        L <- as.list(D$data[M$column_names])
        L[["sep"]] <- M$separator

        # cat columns
        out <- as.character(do.call(paste, L))

        # add prefix
        if (!is.null(M$prefix)) {
            out <- paste0(M$prefix, M$separator, out)
        }
        # add suffix
        if (!is.null(M$suffix)) {
            out <- paste0(out, M$separator, M$suffix)
        }

        # update table
        D$data[, M$output_column] <- out

        # remove old columns if requested
        if (M$clean) {
            torem <- M$column_names

            # dont remove the output column
            w <- which(torem %in% M$output_column)
            if (length(w) > 0) {
                torem <- torem[-w]
            }

            #
            w <- which(colnames(D$data) %in% torem)
            if (length(w) > 0) {
                D$data <- D$data[, -w]
            }
        }

        # update object
        M$updated <- D

        return(M)
    }
)
