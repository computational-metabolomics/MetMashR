#' @eval get_description('unique_records')
#' @export
#' @include annotation_source_class.R
unique_records <- function(...) {
    out <- struct::new_struct(
        "unique_records",
        ...
    )

    return(out)
}


.unique_records <- setClass(
    "unique_records",
    contains = c("model"),
    slots = c(updated = "entity"),
    prototype = list(
        name = "Keep unique_records",
        description = paste0(
            "reduces an annotation source to unique records only; ",
            "all duplicates are removed."
        ),
        type = "unique",
        predicted = "updated",
        .outputs = c("updated"),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The updated annotations as an `annotation_source` object"
            ),
            type = "annotation_source"
        )
    )
)


#' @export
setMethod(
    f = "model_apply",
    signature = c("unique_records", "annotation_source"),
    definition = function(M, D) {
        D$data <- unique(D$data)
        M$updated <- D

        return(M)
    }
)
