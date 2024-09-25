#' @eval get_description('trim_whitespace')
#' @export
#' @include annotation_source_class.R
trim_whitespace <- function(
        column_names,
        which = "both",
        whitespace = "[ \t\r\n]",
        ...) {
    out <- struct::new_struct(
        "trim_whitespace",
        column_names = column_names,
        which = which,
        whitespace = whitespace,
        ...
    )
    return(out)
}

.trim_whitespace <- setClass(
    "trim_whitespace",
    contains = c("model"),
    slots = c(
        column_names = "entity",
        which = "enum",
        whitespace = "entity",
        updated = "entity"
    ),
    prototype = list(
        name = "Trim whitespace",
        description = paste0(
            "A wrapper for [`trimws()`]. Removes leading and/or trailing ",
            "whitespace from character strings."
        ),
        type = "filter",
        predicted = "updated",
        .params = c("column_names", "which", "whitespace"),
        .outputs = c("updated"),
        column_names = entity(
            name = "Column name",
            description = paste0(
                "The column name(s) in the annotation_source to trim ",
                'white space from. Special case ".all" will apply to all ',
                "columns."
            ),
            type = c("character"),
            value = "V1",
            max_length = Inf
        ),
        which = enum(
            name = "Trailing and/or leading whitespace",
            description = c(
                paste0(
                    "A character string specifying the location of ",
                    "whitespace to ",
                    "remove."
                ),
                left = "Remove leading whitespace.",
                right = "Remove trailing whitespace.",
                both = "Remove both leading and trailing whitespace."
            ),
            type = "character",
            max_length = 1,
            allowed = c("left", "right", "both"),
            value = "both"
        ),
        whitespace = entity(
            name = "Whitespace",
            description = paste0(
                'A string specifying a regular expression to match
                (one character of) "white space". See [`trimws()`] for ',
                "details."
            ),
            value = "[ \t\r\n]",
            type = "character",
            max_length = 1
        ),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The annotation_source after trimming whitespace"
            ),
            type = "annotation_source",
            max_length = Inf
        )
    )
)


#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("trim_whitespace", "annotation_source"),
    definition = function(M, D) {
        # get column names
        if (".all" %in% M$column_names) {
            cnames <- colnames(D$data)
        } else {
            cnames <- M$column_names
        }
        
        # get trimws
        trimmed <- lapply(D$data[, cnames], trimws,
                        which = M$which, whitespace = M$whitespace
        )
        
        # update table
        D$data[, cnames] <- trimmed
        
        # update object
        M$updated <- D
        
        return(M)
    }
)
