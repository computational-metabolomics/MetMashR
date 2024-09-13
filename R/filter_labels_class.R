#' @eval get_description('filter_labels')
#' @export
#' @include annotation_source_class.R
filter_labels <- function(
        column_name,
        labels,
        mode = "exclude",
        perl = FALSE,
        fixed = FALSE,
        match_na = FALSE,
        ...) {
    out <- struct::new_struct(
        "filter_labels",
        column_name = column_name,
        labels = labels,
        mode = mode,
        perl = perl,
        fixed = fixed,
        match_na = match_na,
        ...
    )
    return(out)
}



.filter_labels <- setClass(
    "filter_labels",
    contains = c("model"),
    slots = c(
        column_name = "entity",
        labels = "entity",
        mode = "enum",
        filtered = "entity",
        flags = "entity",
        perl = "entity",
        fixed = "entity",
        match_na = "entity"
    ),
    prototype = list(
        name = "Filter by factor labels",
        description = paste0(
            "Removes (or includes) annotations such that the named column
            excludes (or includes) the specified labels."
        ),
        type = "univariate",
        predicted = "filtered",
        .params = c(
            "column_name", "labels", "mode", "perl", "fixed",
            "match_na"
        ),
        .outputs = c("filtered", "flags"),
        column_name = entity(
            name = "Column name",
            description = "The column name to filter.",
            type = c("character"),
            value = "V1",
            max_length = 1
        ),
        labels = entity(
            name = "labels",
            description = paste0(
                "The labels to filter by. Uses `[grepl()]` so regex ",
                "is accepted e.g. for partial matching or labels."
            ),
            type = c("character"),
            value = "",
            max_length = Inf
        ),
        mode = enum(
            name = "Filter mode",
            description = c(
                "exclude" = paste0(
                    "The specified labels are removed from the annotation ",
                    "table."
                ),
                "include" = paste0(
                    "Only the specified labels are retained in the annotation ",
                    "table."
                )
            ),
            type = c("character"),
            value = "exclude",
            max_length = 1,
            allowed = c("exclude", "include")
        ),
        filtered = entity(
            name = "Filtered annotations",
            description = "The annotation_source after filtering.",
            type = "annotation_source",
            max_length = Inf
        ),
        flags = entity(
            name = "Flags",
            description = paste0(
                "A list of flags indicating which annotations had a matching ",
                "label"
            ),
            value = data.frame(),
            type = "data.frame",
            max_length = Inf
        ),
        perl = entity(
            name = "Use perl",
            description = "Use a Perl-compatible regex.",
            value = FALSE,
            type = "logical"
        ),
        fixed = entity(
            name = "Fixed match",
            description = "Use exact matching.",
            value = FALSE,
            type = "logical"
        ),
        match_na = entity(
            name = "Match NA",
            description = c(
                "TRUE" = paste0(
                    "NA values will be treated as if they ",
                    "matched to one of the labels."
                ),
                "FALSE" = paste0(
                    "NA values will be treated as though they ",
                    "did not match to any of the labels."
                )
            ),
            value = FALSE,
            type = "logical"
        )
    )
)


#' @export
setMethod(
    f = "model_apply",
    signature = c("filter_labels", "annotation_source"),
    definition = function(M, D) {
        X <- D$data
        
        if (nrow(X) == 0) {
            M$filtered <- D
            return(M)
        }
        
        # convert to char for comparison
        X[[M$column_name]] <- as.character(X[[M$column_name]])
        
        # flag if found
        G <- lapply(M$labels, grepl,
                    x = X[[M$column_name]], perl = M$perl,
                    fixed = M$fixed
        )
        names(G) <- M$labels
        G <- as.data.frame(G)
        IN <- apply(G, 1, any)
        
        # set NA to true if requested
        if (M$match_na) {
            IN <- IN | is.na(X[[M$column_name]])
        }
        
        if (M$mode == "include") {
            w <- which(IN)
        } else {
            w <- which(!IN)
        }
        
        flags <- data.frame(G, flag = IN, value = X[[M$column_name]])
        rownames(flags) <- rownames(X)
        
        M$flags <- flags
        
        # keep flagged
        X <- X[w, ]
        
        D$data <- X
        
        M$filtered <- D
        
        return(M)
    }
)
