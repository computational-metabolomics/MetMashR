#' @eval get_description('MetFrag_source')
#' @include lcms_table_class.R
#' @family {annotation tables}
#' @export MetFrag_source
MetFrag_source <- function(source,
                           tag = "MetFrag",
                           ...) {
    # new object
    out <- new_struct(
        "MetFrag_source",
        source = source,
        tag = tag,
        ...
    )
    return(out)
}


.MetFrag_source <- setClass(
    "MetFrag_source",
    contains = c("lcms_table")
)

#' @export
setMethod(
    f = "model_apply",
    signature = c("MetFrag_source", "annotation_source"),
    definition = function(M, D) {
        # read in the file
        MF <- read.table(
            file = M$source, sep = "\t",
            header = TRUE
        )

        # add ids
        MF$id <- as.character(seq_len(nrow(MF)))

        # add extra columns if requested
        if (length(M$add_cols) > 0) {
            for (g in seq_len(length(M$add_cols))) {
                MF[[names(M$add_cols)[g]]] <- M$add_cols[[g]]
            }
        }

        D$data <- MF

        D$mz_column <- "parentMZ"
        D$tag <- M$tag
        D$rt_column <- "parentRT"
        D$id_column <- "id"

        M$imported <- D

        return(M)
    }
)
