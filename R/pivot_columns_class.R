#' @eval get_description('pivot_columns')
#' @export
#' @include annotation_source_class.R
pivot_columns <- function(
        column_groups,
        group_labels,
        ...) {
    out <- struct::new_struct(
        "pivot_columns",
        column_groups = column_groups,
        group_labels = group_labels,
        ...
    )
    
    return(out)
}


.pivot_columns <- setClass(
    "pivot_columns",
    contains = c("model"),
    slots = c(
        updated = "entity",
        column_groups = "entity",
        group_labels = "entity"
    ),
    prototype = list(
        name = "Pivot longer",
        description = paste0(
            "Combine multiple groups of columns into a single group of ",
            "columns with group labels."
        ),
        type = "pivot",
        predicted = "updated",
        .params = c("group_labels", "column_groups"),
        .outputs = c("updated"),
        libraries = "dplyr",
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The updated annotations as an `annotation_source` object"
            ),
            type = "annotation_source"
        ),
        group_labels = entity(
            name = "Group labels",
            description = paste0(
                "A named list of columns and the label to use for all records ",
                "in that column."
            ),
            type = "list",
            value = list()
        ),
        column_groups = entity(
            name = "Column groups",
            description = paste0(
                "A named list of columns to group together into a single ",
                "group of columns. There should be the same number of columns ",
                "in each group."
            ),
            type = "list"
        )
    )
)


#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("pivot_columns", "annotation_source"),
    definition = function(M, D) {
        # get columns common to all groups i.e those not named in a group
        common_cols <- 
            colnames(D$data)[!(colnames(D$data) %in% unlist(M$column_groups))]
        
        L <- list()
        # for each column group
        for (cg in names(M$column_groups)) {
            # get subset of columns
            group_df <- select(
                D$data,
                all_of(c(common_cols, M$column_groups[[cg]]))
            )
            # add group labels
            for (k in names(M$group_labels)) {
                labels <- M$group_labels[[k]]
                w <- which(names(labels) == cg)
                group_df[[k]] <- labels[w]
            }
            L[[cg]] <- group_df
        }
        L <- do.call(rbind, L)
        D$data <- L
        M$updated <- D
        return(M)
    }
)
