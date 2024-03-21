#' @eval get_description('combine_sources')
#' @export
#' @include annotation_source_class.R
combine_sources <- function(source_list,
                            matching_columns = NULL,
                            keep_cols = NULL,
                            source_col = "annotation_source",
                            exclude_cols = NULL,
                            tag = "combined",
                            as = annotation_source(
                                name = "combined",
                                description = paste0(
                                    "A source created by combining two or ",
                                    "more sources"
                                )
                            ),
                            ...) {
    # if source list is an annotation_source, make it a list
    if (is(source_list, "annotation_source")) {
        source_list <- list(source_list)
    }

    # check fcns are all functions
    if (is(source_list, "list")) {
        if (length(source_list) > 0) {
            check <- all(unlist(lapply(source_list, is,
                class2 = "annotation_source"
            )))
            if (!check) {
                stop("all source_list items must be annotation_source objects.")
            }
        }
    }

    out <- struct::new_struct(
        "combine_sources",
        source_list = source_list,
        matching_columns = matching_columns,
        keep_cols = keep_cols,
        source_col = source_col,
        exclude_cols = exclude_cols,
        tag = tag,
        as = as,
        ...
    )
    return(out)
}



.combine_sources <- setClass(
    "combine_sources",
    contains = c("model"),
    slots = c(
        source_list = "entity",
        matching_columns = "entity",
        keep_cols = "entity",
        source_col = "entity",
        combined_table = "entity",
        exclude_cols = "entity",
        tag = "entity",
        as = "entity"
    ),
    prototype = list(
        name = "Combine annotation sources (tables)",
        description = paste0(
            "Annotation tables are joined and matching columns merged."
        ),
        type = "univariate",
        predicted = "combined_table",
        .params = c(
            "source_list", "matching_columns", "keep_cols",
            "source_col", "exclude_cols", "tag", "as"
        ),
        .outputs = c("combined_table"),
        combined_table = entity(
            name = "Combined annotation tables",
            description =
                "The annotation tabel after combining the input tables.",
            type = "annotation_source"
        ),
        source_list = entity(
            name = "Source list",
            description = "A list of annotation sources to be combined.",
            type = "list"
        ),
        matching_columns = entity(
            name = "Matching columns",
            description = paste0(
                "A named vector of columns names to be created by merging ",
                "columns from individual sources. e.g. `c('hello'='world')` ",
                "will rename the ",
                "'hello' column to 'world' if found in any of the tables."
            ),
            type = c("character", "NULL"),
            value = NULL
        ),
        keep_cols = entity(
            name = "Keep columns",
            description = paste0(
                "A list of column names to keep in the combined table ",
                "(padded with NA) if detected in one of the input tables. ",
                'Special case ".all" will keep all columns from all tables.'
            ),
            type = c("character", "NULL"),
            value = NULL
        ),
        source_col = entity(
            name = "Source column",
            description = paste0(
                "The column name that will be created to contain ",
                "a tag to indicate which source the annotation originated ",
                "from."
            ),
            type = "character",
            value = "annotation_source"
        ),
        exclude_cols = entity(
            name = "Exclude columns",
            description = paste0(
                "Column names to be excluded from the merged ",
                "annotation table. Note this is applied after `keep_cols`."
            ),
            type = c("NULL", "character"),
            value = NULL
        ),
        tag = entity(
            name = "New tag",
            description = paste0("The tag given to the newly combined table."),
            type = c("character"),
            value = "combined"
        ),
        as = entity(
            name = "Output object",
            description = paste0(
                "An annotation_source object to use as the base class for the ",
                "combined sources."
            ),
            value = annotation_source(),
            type = "annotation_source"
        )
    )
)



#' @export
setMethod(
    f = "model_apply",
    signature = c("combine_sources", "annotation_source"),
    definition = function(M, D) {
        A <- c(M$source_list, D) # makes a list
        M$combined_table <- vertical_join(
            x = A,
            matching_columns = M$matching_columns,
            keep_cols = M$keep_cols,
            source_col = M$source_col,
            exclude_cols = M$exclude_cols,
            as = M$as
        )
        M$combined_table$tag <- M$tag

        return(M)
    }
)


#' @export
setMethod(
    f = "model_apply",
    signature = c("combine_sources", "list"),
    definition = function(M, D) {
        # make a list
        A <- c(M$source_list, D)

        # join
        M$combined_table <- vertical_join(
            x = A,
            matching_columns = M$matching_columns,
            keep_cols = M$keep_cols,
            source_col = M$source_col,
            exclude_cols = M$exclude_cols, as = M$as
        )

        # update tag
        M$combined_table$tag <- M$tag

        return(M)
    }
)
