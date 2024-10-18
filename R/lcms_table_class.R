#' @eval get_description("lcms_table")
#' @include annotation_table_class.R
#' @export lcms_table
#' @family annotation_tables
lcms_table <- function(
        data = NULL,
        tag = "",
        id_column = "id",
        mz_column = "mz",
        rt_column = "rt",
        ...) {
    
    if (is.null(data)) {
        data = data.frame()
    }
    
    if (nrow(data)==0 & ncol(data)==0) {
        data <- data.frame(
            id = character(0),
            mz = numeric(0),
            rt = numeric(0)
        )
        colnames(data) <- c(
            id_column,
            mz_column,
            rt_column
        )
    }
    
    
    
    # new object
    out <- new_struct(
        "lcms_table",
        data = data,
        tag = tag,
        mz_column = mz_column,
        rt_column = rt_column,
        id_column = id_column,
        .required = c(mz_column, id_column, rt_column),
        ...
    )
    return(out)
}

.lcms_table <- setClass(
    "lcms_table",
    contains = c("annotation_table"),
    slots = c(
        mz_column = "entity",
        rt_column = "entity"
    ),
    prototype = list(
        name = "LCMS table",
        description = paste0(
            "An LCMS table extends [`annotation_table()`] to represent ",
            "annotation data for an LCMS experiment. Columns representing ",
            "m/z and retention time are required for an `lcms_table`."
        ),
        mz_column = entity(
            name = "m/z column name",
            description = paste0(
                "The column name of the annotation data.frame containing m/z ",
                "values."
            ),
            type = "character",
            max_length = 1,
            value = "mz"
        ),
        rt_column = entity(
            name = "Retention time column name",
            description = paste0(
                "The column name of the annotation data.frame containing ",
                "retention time values."
            ),
            type = "character",
            max_length = 1,
            value = "rt"
        ),
        .params = c("mz_column", "rt_column"),
        .required = c("mz_column", "rt_column")
    )
)
