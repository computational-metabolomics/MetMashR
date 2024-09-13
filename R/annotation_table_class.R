#' @eval get_description("annotation_table")
#' @include annotation_source_class.R
#' @family {annotation tables}
#' @family {annotation sources}
#' @export
annotation_table <- function(
        data = data.frame(),
        tag = "",
        id_column = NULL,
        ...) {
    if (is.null(id_column)) {
        data$.MetMashR_id <- seq_len(nrow(data))
        id_column <- ".MetMashR_id"
    }
    
    # new object
    out <- new_struct(
        "annotation_table",
        data = data,
        id_column = id_column,
        tag = tag,
        .required = id_column,
        ...
    )
    
    return(out)
}

.annotation_table <- setClass(
    "annotation_table",
    contains = c("annotation_source"),
    slots = c(
        id_column = "entity"
    ),
    prototype = list(
        name = "An annotation table",
        description = paste0(
            "An `annotation_table` is an [`annotation_source()`] where the ",
            "imported data.frame contains measured experimental data. An
            `id_column` of values is required to uniquely indentify each ",
            "record (row) in the ",
            "table (NB these are NOT molecule identifiers, which may be ",
            "be present in multiple records)."
        ),
        id_column = entity(
            name = "Annotation row identifiers",
            description = paste0(
                "The column name of the annotation data.frame containing row ",
                "identifers. If NULL This will be generated automatically."
            ),
            type = "character",
            max_length = 1,
            value = "id"
        ),
        data = .set_entity_value(
            "annotation_source",
            param_id = "data",
            value = data.frame(
                id = character(0)
            )
        ),
        .params = "id_column",
        .required = "id"
    )
)
