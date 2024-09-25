#' @eval get_description('compute_record')
#' @export
#' @include annotation_source_class.R
compute_record <- function(
        fcn,
        ...) {
    out <- struct::new_struct(
        "compute_record",
        fcn = fcn,
        ...
    )
    
    return(out)
}


.compute_record <- setClass(
    "compute_record",
    contains = c("model"),
    slots = c(
        updated = "entity",
        fcn = "entity"
    ),
    prototype = list(
        name = "Compute a value for a record",
        description = paste0(
            "Compute values for a record based on other values in a record"
        ),
        type = "compute",
        predicted = "updated",
        .params = c("fcn"),
        .outputs = c("updated"),
        libraries = "dplyr",
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The updated annotations as an `annotation_source` object"
            ),
            type = "annotation_source"
        ),
        fcn = entity(
            name = "Function",
            description = paste0(
                "The function used to compute the values for the record."
            ),
            type = "function"
        )
    )
)


#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("compute_record", "annotation_source"),
    definition = function(M, D) {
        for (k in seq_len(nrow(D$data))) {
            D$data[k, ] <- M$fcn(D$data[k, , drop = FALSE])
        }
        
        M$updated <- D
        return(M)
    }
)
