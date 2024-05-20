#' @eval get_description('filter_venn')
#' @export
#' @include annotation_source_class.R
filter_venn <- function(
        factor_name,
        group_column = NULL,
        tables = NULL,
        levels,
        mode = "exclude",
        perl = FALSE,
        fixed = FALSE,
        ...) {
    out <- struct::new_struct(
        "filter_venn",
        factor_name = factor_name,
        group_column = group_column,
        tables = tables,
        levels = levels,
        mode = mode,
        perl = perl,
        fixed = fixed,
        ...
    )
    return(out)
}



.filter_venn <- setClass(
    "filter_venn",
    contains = c("model"),
    slots = c(
        factor_name = "entity",
        group_column = "entity",
        tables = "entity",
        levels = "entity",
        mode = "enum",
        filtered = "entity",
        flags = "entity",
        perl = "entity",
        fixed = "entity"
    ),
    prototype = list(
        name = "Filter by factor levels",
        description = paste0(
            "Removes (or includes) annotations such that the named column ",
            "excludes (or includes) the specified levels."
        ),
        type = "univariate",
        predicted = "filtered",
        .params = c(
            "factor_name", "group_column", "tables", "levels", "mode",
            "perl", "fixed"
        ),
        .outputs = c("filtered", "flags"),
        factor_name = entity(
            name = "Factor name",
            description = paste0(
                "The name of the column(s) in the `annotation_source` to ",
                "generate a chart from. Up to seven columns can be compared ",
                "for a single `annotation_source`"
            ),
            type = "character",
            value = "V1",
            max_length = 7
        ),
        group_column = entity(
            name = "Grouping column",
            description = paste0(
                "The name of the column in the `annotation_source` to ",
                "create groups from in the Venn diagram. This parameter is ",
                "ignored if `!is.null(tables)`, as each table is ",
                "considered to be a group. This parameter is also ignored if ",
                "more than one `factor_name` is provided, as each column is ",
                "considered a group."
            ),
            type = c("character", "NULL"),
            value = NULL,
            max_length = 1
        ),
        tables = entity(
            name = "Tables",
            description = paste0(
                "A list of `annotation_sources` to generate the venn groups ",
                "from. If the only table of interest is the table coming in ",
                "from ",
                "`model_apply` then set `tables = NULL` and use `group_column`."
            ),
            type = c("list", "NULL"),
            value = NULL,
            max_length = Inf
        ),
        levels = entity(
            name = "Levels",
            description = "The venn diagram levels to filter by.",
            type = c("character"),
            value = "",
            max_length = Inf
        ),
        mode = enum(
            name = "Filter mode",
            description = c(
                "exclude" = paste0(
                    "The specified levels are removed from the annotation ",
                    "table."
                ),
                "include" = paste0(
                    "Only the specified levels are retained in the ",
                    "annotation table."
                )
            ),
            type = c("character"),
            value = "exclude",
            max_length = 1,
            allowed = c("exclude", "include")
        ),
        filtered = entity(
            name = "Filtered annotations",
            description = "annotation_source after filtering.",
            type = "annotation_source",
            max_length = Inf
        ),
        flags = entity(
            name = "Flags",
            description = paste0(
                "A list of flags indicating which annotations were removed."
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
        )
    )
)


#' @export
setMethod(
    f = "model_apply",
    signature = c("filter_venn", "annotation_source"),
    definition = function(M, D) {
        # tables
        L <- M$tables
        
        # if we got more than one table...
        if (length(L) > 0) {
            # gather all annotation_sources
            L <- c(list(D), L)
            
            # if only one column name, assume same column in all sources
            if (length(M$factor_name) == 1) {
                M$factor_name <- rep(M$factor_name, length(L))
            }
            
            # check we have a column for all sources
            if (length(M$factor_name) != length(L)) {
                stop(
                    "You must provide either a single factor_name ",
                    "present in all sources, or provide a factor_name ",
                    "for each source.\n"
                )
                M$factor_name <- M$factor_name[1]
            }
            
            # get tags
            tags <- lapply(L, param_value, name = "tag")
            names(L) <- tags
            
            # get tables
            L <- lapply(L, param_value, name = "data")
            
            # get columns
            L <- mapply("[[", L, M$factor_name)
        } else if (length(M$factor_name) > 1) {
            # comparing multiple columns
            L <- as.list(D$data[M$factor_name])
        } else {
            # if we only got one table and one factor...
            u <- unique(D$data[[M$group_column]])
            
            # construct list for Venn
            L <- list()
            for (k in u) {
                this <- D$data[[M$factor_name]]
                L[[k]] <- this[D$data[[M$group_column]] == k]
            }
        }
        
        # max 7(!) groups
        if (length(L) > 7) {
            stop(
                "Venn chart can only be plotted for up to 7 groups. ",
                'Try using "Upset" plots (annotation_upset_chart) ',
                "instead."
            )
        }
        
        # process venn
        this <- ggVennDiagram::process_data(RVenn::Venn(L))
        
        # get regions
        r <- ggVennDiagram::venn_region(this)
        
        # add region flags
        D$data[[".filter_venn"]] <- NA
        for (k in seq_len(nrow(r))) {
            # rows in table in region
            w <- which(D$data[[M$factor_name[1]]] %in% r$item[[k]])
            # add flags
            D$data[[".filter_venn"]][w] <- r$name[k]
        }
        
        # filter
        M2 <- filter_labels(
            column_name = ".filter_venn",
            labels = M$levels,
            mode = M$mode,
            perl = M$perl,
            fixed = M$fixed
        )
        M2 <- model_apply(M2, D)
        
        # remove extra column
        D2 <- predicted(M2)
        D2$data$.filter_venn <- NULL
        
        # update object
        M$filtered <- D2
        M$flags <- M2$flags
        
        return(M)
    }
)
