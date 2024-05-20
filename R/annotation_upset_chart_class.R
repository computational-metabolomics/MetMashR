
#' @export
annotation_upset_chart <- function(
        factor_name,
        group_column,
        width_ratio = 0.2,
        xlabel = 'group',
        sort_intersections = 'descending',
        intersections = 'observed',
        n_intersections = NULL,
        min_size = 0,
        queries = list(),
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
        ...
    )
    
    return(out)
}


.annotation_upset_chart <- setClass(
    "annotation_upset_chart",
    contains = "chart",
    slots = c(
        factor_name = 'entity',
        group_column = 'entity',
        width_ratio = 'entity',
        xlabel = 'entity',
        sort_intersections = 'entity',
        intersections = 'entity',
        n_intersections = 'entity',
        min_size = 'entity',
        queries = 'entity'
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
            "sort_intersections", "intersections", "n_intersections", "min_size",
            "queries"
        ),
        libraries = "ComplexUpset",
        factor_name = entity(
            name = "Factor name",
            description = paste0(
                "The name of the column in the `annotation_source` to ",
                "generate an UpSet chart from."
            ),
            type = "character",
            value = "V1",
            max_length = 1
        ),
        group_column = entity(
            name = "Grouping column",
            description = paste0(
                'A column of group/set labels'),
            value = "V2",
            max_length = 1,
            type = "character"
        ),
        width_ratio = entity(
            name = "Width ratio",
            description = paste0(
                'Proportion of plot given to set size bar chart.'
            ),
            type = "numeric",
            value = 0.2,
            max_length = 1
        ),
        xlabel = entity(
            name = "X-axis label",
            description = paste0(
                'The label used for the x-axis.'
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
            value = 'descending',
            max_length = 1,
            allowed=c('ascending','descending','none')
        ),
        intersections = entity(
            name = "Intersections",
            description = paste0(
                "The intersections to include in the plot."
            ),
            type = c("character",'list'),
            value = 'observed',
            max_length = Inf
        ),
        n_intersections = entity(
            name = "Number of intersections",
            description = paste0(
                'The number of intersections to include in the plot.'
            ),
            value = NULL,
            type = c("numeric", "integer","NULL"),
            max_length = 1
        ),
        min_size = entity(
            name = "Minimum size",
            description = paste0(
                'The minimum size of an intersection for it to be included in the plot.'
            ),
            value = 0,
            type = c("numeric", "integer"),
            max_length = 1
        ),
        queries = entity(
            name = "Queries",
            description = paste0(
                'A list of upset queries.'
            ),
            value = list(),
            type = c("list"),
            max_length = Inf
        )
    )
)

#' @export
setMethod(
    f = "chart_plot",
    signature = c("annotation_upset_chart", "annotation_source"),
    definition = function(obj, dobj) {
        
        L = levels(factor(dobj$data[[obj$group_column]]))
        U = unique(dobj$data[[obj$factor_name]])
        G = lapply(U,function(x){
            w = which(dobj$data[[obj$factor_name]]==x)
            g = dobj$data[[obj$group_column]][w]
            out = L %in% g
        })
        G = as.data.frame(do.call(rbind,G))
        colnames(G) = L
        rownames(G) = U
        
        srt = obj$sort_intersections
        if (srt=='none') {
            srt = FALSE
        }
        
        g = ComplexUpset::upset(
            G, 
            colnames(G), 
            name = obj$xlabel, 
            width_ratio = obj$width_ratio,
            sort_intersections = srt,
            intersections = obj$intersections,
            n_intersections = obj$n_intersections,
            min_size = obj$min_size,
            queries = obj$queries)
        
        return(g)
    }
)
