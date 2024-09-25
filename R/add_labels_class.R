#' @eval get_description('add_labels')
#' @export
#' @include annotation_source_class.R
add_labels <- function(labels, replace = FALSE, ...) {
    if (is.character(labels)) {
        labels <- as.list(labels)
    }

    out <- struct::new_struct(
        "add_labels",
        labels = labels,
        replace = replace,
        ...
    )

    return(out)
}


.add_labels <- setClass(
    "add_labels",
    contains = c("model"),
    slots = c(
        updated = "entity",
        replace = "entity",
        labels = "entity"
    ),
    prototype = list(
        name = "Add column of labels",
        description = paste0(
            "Adds new columns with the specified labels for each record."
        ),
        type = "add labels",
        predicted = "updated",
        .params = c("labels", "replace"),
        .outputs = c("updated"),
        libraries = "dplyr",
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The updated annotations as an `annotation_source` object"
            ),
            type = "annotation_source"
        ),
        labels = entity(
            name = "labels",
            description = paste0(
                "A named list of columns and the label to use for all records ",
                "in that column."
            ),
            type = "list",
            value = list()
        ),
        replace = entity(
            name = "Replace columns",
            description = c(
                "TRUE" = paste0(
                    "If present, the new columns will replace existing ",
                    "columns in the source data.frame"
                ),
                "FALSE" = paste0(
                    "An error will be thrown if the new columns are already ",
                    "in the source data.frame."
                )
            ),
            type = "logical"
        )
    )
)

setValidity("add_labels", method = function(object) {
    check = FALSE
    if (length(object$labels) > 0) {
        check <- is.null(names(object$labels))
    }
    if (check) {
        msg <- "the output of names(labels) must not be NULL"
    } else {
        msg <- TRUE
    }
    return(msg)
})

#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("add_labels", "annotation_source"),
    definition = function(M, D) {
        check <- check_for_columns(D, names(M$labels), msg = FALSE)

        if (any(check) & !M$replace) {
            stop(
                'The columns being added by the "add_labels" object ',
                "are already present in the data.frame."
            )
        }

        for (k in seq_len(length(M$labels))) {
            n <- names(M$labels)[k]
            D$data[[n]] <- M$labels[[n]]
        }
        M$updated <- D

        return(M)
    }
)
