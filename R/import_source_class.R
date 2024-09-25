#' @eval get_description('import_source')
#' @export
#' @include annotation_source_class.R
import_source <- function(...) {
    out <- struct::new_struct(
        "import_source",
        ...
    )
    return(out)
}

.import_source <- setClass(
    "import_source",
    contains = c("model"),
    slots = c(
        imported = "entity"
    ),
    prototype = list(
        name = "Import_source",
        description = paste0(
            "A wrapper for [`read_source()`] that can be used in an ",
            "annotation workflow to import an annotation source. "
        ),
        type = "import",
        predicted = "imported",
        # .params=c(),
        .outputs = c("imported"),
        imported = entity(
            name = "Imported annotation_source",
            description = "The `annotation_source` after importing the data.",
            type = "annotation_source",
            max_length = 1
        )
    )
)


#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("import_source", "annotation_source"),
    definition = function(M, D) {
        M$imported <- read_source(D)

        return(M)
    }
)
