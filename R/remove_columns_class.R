#' @eval get_description('remove_columns')
#' @export
#' @include annotation_source_class.R
#' @seealso [dplyr::select()]
#' @seealso [tidyselect::eval_select()]
#' @import rlang
remove_columns <- function(expression = everything(), ...) {
    # capture
    ex <- rlang::enexpr(expression)

    out <- struct::new_struct(
        "remove_columns",
        expression = ex,
        ...
    )
    return(out)
}


.remove_columns <- setClass(
    "remove_columns",
    contains = c("model"),
    slots = c(
        updated = "entity",
        expression = "entity"
    ),
    prototype = list(
        name = "Select columns",
        description = paste0(
            "A wrapper around [`tidyselect::eval_select`]. Remove columns ",
            "from an annotation table using tidy grammar."
        ),
        type = "remove",
        predicted = "updated",
        .params = c("expression"),
        .outputs = c("updated"),
        libraries = c("tidyselect", "rlang"),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The updated annotations as an `annotation_source` object"
            ),
            type = "annotation_source"
        ),
        expression = entity(
            name = "Select columns expression",
            description = paste0(
                'A valid rlang::expr for tidy evaluation via eval_select. e.g.
                `expression = all_of(c("foo","bar"))` will select columns
                named "foo" and "bar" from the annotation data.frame.
                '
            ),
            value = expr(tidyselect::everything()),
            type = "call"
        )
    )
)


#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("remove_columns", "annotation_source"),
    definition = function(M, D) {
        # column indexes matching expression
        loc <- tidyselect::eval_select(M$expression, data = D$data)

        # update names
        D$data <- D$data[, -loc]

        # update object
        M$updated <- D

        return(M)
    }
)
