#' @eval get_description('split_column')
#' @export
#' @include annotation_source_class.R
split_column <- function(
        column_name,
        separator = "_",
        padding = NA,
        keep_indices = NULL,
        clean = TRUE,
        ...) {
    out <- struct::new_struct(
        "split_column",
        column_name = column_name,
        separator = separator,
        padding = padding,
        clean = clean,
        keep_indices = keep_indices,
        ...
    )
    return(out)
}

.split_column <- setClass(
    "split_column",
    contains = "model",
    slots = c(
        column_name = "entity",
        separator = "entity",
        padding = "entity",
        updated = "entity",
        clean = "entity",
        keep_indices = "entity"
    ),
    prototype = list(
        name = "Split a column",
        description = paste0(
            "A wrapper for [`strsplit`]. Divides a column into multiple ",
            "columns by dividing the contents"
        ),
        type = "processing",
        predicted = "updated",
        .params = c("column_name", "separator", "clean", "padding", 
                    "keep_indices"),
        .outputs = c("updated"),
        column_name = entity(
            name = "Column names",
            description = paste0(
                "The column name in the annotation_source split."
            ),
            type = c("character"),
            value = "V1",
            max_length = Inf
        ),
        separator = entity(
            name = "Separator",
            description = paste0(
                "A substring to split the column by."
            ),
            type = "character",
            max_length = 1,
            value = "_"
        ),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The annotation_source after splitting the column."
            ),
            type = "annotation_source",
            max_length = Inf
        ),
        clean = entity(
            name = "Clean old columns",
            description = c(
                "TRUE" = "The named columns are removed after being split.",
                "FALSE" = "The named columns are retained after being split."
            ),
            value = TRUE,
            type = "logical",
            max_length = 1
        ),
        padding = entity(
            name = "Pad missing values",
            description = paste0(
                "A character string used to represent missing and zero length ",
                "strings after splitting."
            ),
            value = NA,
            type = c("character", "logical"),
            max_length = 1
        ),
        keep_indices = entity(
            name = "Indices to keep",
            description = paste0(
                "The indices of columns to keep after ",
                "splitting. If NULL then all columns are retained."
            ),
            value = NULL,
            type = c("numeric", "integer"),
            max_length = Inf
        )
    )
)


#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("split_column", "annotation_source"),
    definition = function(M, D) {
        # split
        s <- strsplit(D$data[[M$column_name]], M$separator)
        # create new columns
        s <- lapply(s, function(x) {
            x[x == ""] <- M$padding
            df <- as.data.frame(matrix(x, nrow = 1, byrow = TRUE))
            return(df)
        })
        s <- plyr::rbind.fill(s)
        s[is.na(s)] <- M$padding
        
        # keep selected columns by index
        s <- s[, M$keep_indices, drop = FALSE]
        
        # new column names
        colnames(s) <- paste0(M$column_name, "_", seq_len(ncol(s)))
        
        # bind with original data
        s <- cbind(D$data, s)
        
        # clean
        if (M$clean) {
            w <- which(colnames(s) == M$column_name)
            s <- s[, -w]
        }
        D$data <- s
        
        # update object
        M$updated <- D
        
        return(M)
    }
)
