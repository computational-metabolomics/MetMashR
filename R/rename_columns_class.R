#' @eval get_description('rename_columns')
#' @export
#' @include annotation_source_class.R
rename_columns <- function(
        expression,
        ...) {
    # capture
    ex <- rlang::enexpr(expression)
    
    out <- struct::new_struct(
        "rename_columns",
        expression = ex,
        ...
    )
    
    return(out)
}

.rename_columns <- setClass(
    "rename_columns",
    contains = "model",
    slots = c(
        updated = "entity",
        expression = "entity"
    ),
    prototype = list(
        name = "Select columns",
        description = paste0(
            "A wrapper around [`dplyr::rename`]. Rename columns ",
            "from an annotation table using tidy grammar."
        ),
        type = "rename",
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
            name = "Rename columns expression",
            description = paste0(
                'A valid rlang::expr for tidy evaluation e.g.
                `expression = all_of(c("foo"="bar"))` will rename the column
                named "bar" and "foo".
                '
            ),
            value = expr(everything()),
            type = "call"
        )
    )
)


#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("rename_columns", "annotation_source"),
    definition = function(M, D) {
        # column indexes matching expression
        loc <- tidyselect::eval_select(M$expression, data = D$data)
        
        # update names
        n <- colnames(D$data)
        n[loc] <- names(loc)
        colnames(D$data) <- n
        
        # update object
        M$updated <- D
        
        return(M)
    }
)
