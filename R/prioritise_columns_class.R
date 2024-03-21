#' @eval get_description('prioritise_columns')
#' @export
#' @include annotation_source_class.R zzz.R
prioritise_columns <- function(column_names,
                               output_name,
                               source_name,
                               source_tags = column_names,
                               clean = TRUE,
                               ...) {
    out <- struct::new_struct("prioritise_columns",
        column_names = column_names,
        output_name = output_name,
        clean = clean,
        source_name = source_name,
        source_tags = source_tags,
        ...
    )
    return(out)
}



.prioritise_columns <- setClass(
    "prioritise_columns",
    contains = c("model"),
    slots = c(
        column_names = "entity",
        output_name = "entity",
        clean = "entity",
        updated = "entity",
        source_name = "entity",
        source_tags = "entity"
    ),
    prototype = list(
        name = "Combine several columns into a single column.",
        description = paste0(
            "Several columns are merged into a single column. If multiple ",
            "columns contain overlapping values then priority can be given ",
            "columns earlier in the list."
        ),
        type = "univariate",
        predicted = "updated",
        .params = c(
            "column_names", "output_name", "clean", "source_name",
            "source_tags"
        ),
        .outputs = c("updated"),
        column_names = entity(
            name = "Annotion columns names",
            description = "The name(s) of column(s) to be combined.",
            type = c("character"),
            value = "V1",
            max_length = Inf
        ),
        output_name = entity(
            name = "Annotation column name",
            description = "The name of the new column.",
            type = c("character"),
            value = "",
            max_length = 1
        ),
        source_name = entity(
            name = "Source name",
            description = paste0(
                "The column name used to indicate the where ",
                "the merged values originated."
            ),
            type = "character",
            value = "source_name",
            max_length = 1
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
        ),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The input annotation source with the newly generated column."
            ),
            type = "annotation_source"
        ),
        source_tags = entity(
            name = "Source tags",
            description = paste0(
                "The tags used to identify the source of ",
                "each item in the new column. A tag should be provided for ",
                "each column_name. By default the column name is used."
            ),
            type = "character",
            value = "x",
            max_length = Inf
        )
    )
)


#' @export
setMethod(
    f = "model_apply",
    signature = c("prioritise_columns", "annotation_source"),
    definition = function(M, D) {
        X <- D$data

        # M=check_unique('source_name',colnames(X),M)
        # M=check_unique('output_name',colnames(X),M)

        # the new column
        new_column <- X[, M$column_names[1], drop = TRUE]
        selected <- rep(M$source_tags[1], length(new_column))

        # for each column
        for (k in 2:length(M$column_names)) {
            # replace NA with next column
            w <- which(is.na(new_column))
            new_column[w] <- X[w, M$column_names[k], drop = TRUE]
            selected[w] <- M$source_tags[k]
        }

        X[[M$output_name]] <- new_column
        X[[M$source_name]] <- selected

        if (M$clean) {
            w <- which((colnames(X) %in% M$column_names) &
                !(colnames(X) %in% M$output_name))
            X <- X[, -w]
        }
        D$data <- X
        M$updated <- D

        return(M)
    }
)
