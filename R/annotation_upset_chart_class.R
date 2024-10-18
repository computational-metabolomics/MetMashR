#' @eval get_description('annotation_upset_chart')
#' @export
annotation_upset_chart <- function(factor_name,
    group_column = NULL,
    width_ratio = 0.2,
    xlabel = "group",
    sort_intersections = "descending",
    intersections = "observed",
    n_intersections = NULL,
    min_size = 0,
    queries = list(),
    keep_empty_groups = FALSE,
    ...) {
    out <- struct::new_struct(
        "annotation_upset_chart",
        factor_name = factor_name,
        group_column = group_column,
        width_ratio = width_ratio,
        xlabel = xlabel,
        sort_intersections = sort_intersections,
        intersections = intersections,
        n_intersections = n_intersections,
        min_size = min_size,
        queries = queries,
        keep_empty_groups = keep_empty_groups,
        ...
    )

    return(out)
}


.annotation_upset_chart <- setClass(
    "annotation_upset_chart",
    contains = "chart",
    slots = c(
        factor_name = "entity",
        group_column = "entity",
        width_ratio = "entity",
        xlabel = "entity",
        sort_intersections = "entity",
        intersections = "entity",
        n_intersections = "entity",
        min_size = "entity",
        queries = "entity",
        keep_empty_groups = "entity"
    ),
    prototype = list(
        name = "Annotation UpSet chart",
        description = paste0(
            "Display an UpSet chart of labels in the specified column of an ",
            "annotation_source."
        ),
        type = "image",
        .params = c(
            "factor_name", "group_column", "width_ratio", "xlabel",
            "sort_intersections", "intersections", "n_intersections",
            "min_size",
            "queries", "keep_empty_groups"
        ),
        libraries = "ComplexUpset",
        factor_name = entity(
            name = "Factor name",
            description = paste0(
                "The name of the column(s) in the `annotation_source`(s) to ",
                "generate an UpSet chart from."
            ),
            type = "character",
            value = "V1",
            max_length = Inf
        ),
        group_column = entity(
            name = "Grouping column",
            description = paste0(
                "The name of the column in the `annotation_source` to ",
                "create groups from in the Venn diagram. This parameter is ",
                "ignored if there are multiple input tables, as each table is ",
                "considered to be a group. This parameter is also ignored if ",
                "more than one `factor_name` is provided, as each column is ",
                "considered a group."
            ),
            type = c("character", "NULL"),
            value = NULL,
            max_length = 1
        ),
        width_ratio = entity(
            name = "Width ratio",
            description = paste0(
                "Proportion of plot given to set size bar chart."
            ),
            type = "numeric",
            value = 0.2,
            max_length = 1
        ),
        xlabel = entity(
            name = "X-axis label",
            description = paste0(
                "The label used for the x-axis."
            ),
            type = "character",
            value = "group",
            max_length = 1
        ),
        sort_intersections = enum(
            name = "Sort intersections",
            description = c(
                "ascending" = "Groups are sorted by increasing size.",
                "descending" = "Groups are sorted by decreasing size.",
                "none" = "Groups are not sorted"
            ),
            type = "character",
            value = "descending",
            max_length = 1,
            allowed = c("ascending", "descending", "none")
        ),
        intersections = entity(
            name = "Intersections",
            description = paste0(
                "The intersections to include in the plot."
            ),
            type = c("character", "list"),
            value = "observed",
            max_length = Inf
        ),
        n_intersections = entity(
            name = "Number of intersections",
            description = paste0(
                "The number of intersections to include in the plot."
            ),
            value = NULL,
            type = c("numeric", "integer", "NULL"),
            max_length = 1
        ),
        min_size = entity(
            name = "Minimum size",
            description = paste0(
                "The minimum size of an intersection for it to be included ",
                "in the plot."
            ),
            value = 0,
            type = c("numeric", "integer"),
            max_length = 1
        ),
        queries = entity(
            name = "Queries",
            description = paste0(
                "A list of upset queries."
            ),
            value = list(),
            type = c("list"),
            max_length = Inf
        ),
        keep_empty_groups = entity(
            name = "Keep empty sets",
            description = paste0(
                "whether empty sets should be kept (including sets which are ",
                "only empty after filtering by size)"
            ),
            value = FALSE,
            type = c("logical"),
            max_length = 1
        )
    )
)

#' @export
#' @template chart_plot
setMethod(
    f = "chart_plot",
    signature = c("annotation_upset_chart", "annotation_source"),
    definition = function(obj, dobj, ...) {
        L <- list(...)

        if (length(L) > 0) {
            # more than one dobj
            L <- c(dobj, L)

            # if only one column name, assume same column in all sources
            if (length(obj$factor_name) == 1) {
                obj$factor_name <- rep(obj$factor_name, length(L))
            }

            # check we have a column for all sources
            if (length(obj$factor_name) != length(L)) {
                stop(
                    "You must provide either a single factor_name ",
                    "present in all sources, or provide a factor_name ",
                    "for each source.\n"
                )
            }

            # get tags
            tags <- lapply(L, param_value, name = "tag")
            names(L) <- tags
            L <- lapply(L, param_value, name = "data")

            # get columns
            L <- mapply("[[", L, obj$factor_name)

            ## create upset table
            # unique across all sets
            u <- unique(unlist(L))
            G <- lapply(L, function(x) {
                return(u %in% x)
            })
            G <- do.call(rbind, G)
            colnames(G) <- u
            G <- as.data.frame(t(G))
        } else if (length(obj$factor_name) > 1) {
            # comparing multiple columns
            L <- as.list(dobj$data[obj$factor_name])
            # unique across all sets
            u <- unique(unlist(L))
            G <- lapply(L, function(x) {
                return(u %in% x)
            })
            G <- do.call(rbind, G)
            colnames(G) <- u
            G <- as.data.frame(t(G))
        } else {
            # only dobj provided
            L <- levels(factor(dobj$data[[obj$group_column]]))
            U <- unique(dobj$data[[obj$factor_name]])
            G <- lapply(U, function(x) {
                w <- which(dobj$data[[obj$factor_name]] == x)
                g <- dobj$data[[obj$group_column]][w]
                out <- L %in% g
            })
            G <- as.data.frame(do.call(rbind, G))
            colnames(G) <- L
            rownames(G) <- U
        }

        srt <- obj$sort_intersections
        if (srt == "none") {
            srt <- FALSE
        }

        g <- ComplexUpset::upset(
            G,
            colnames(G),
            name = obj$xlabel,
            width_ratio = obj$width_ratio,
            sort_intersections = srt,
            intersections = obj$intersections,
            n_intersections = obj$n_intersections,
            min_size = obj$min_size,
            queries = obj$queries,
            keep_empty_groups = obj$keep_empty_groups
        )

        return(g)
    }
)

#' @export
#' @template chart_plot
setMethod(
    f = "chart_plot",
    signature = c("annotation_upset_chart", "list"),
    definition = function(obj, dobj) {
        L <- c(obj, dobj)
        names(L)[1] <- "obj"
        names(L)[2] <- "dobj"

        g <- do.call(chart_plot, L)
        return(g)
    }
)
