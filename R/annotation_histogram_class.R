#' @eval get_description('annotation_histogram')
#' @export
annotation_histogram <- function(
        factor_name,
        bins = 30,
        bin_edge = "grey",
        bin_fill = "lightgrey",
        vline = NULL,
        vline_colour = "red",
        ...) {
    out <- struct::new_struct(
        "annotation_histogram",
        factor_name = factor_name,
        bins = bins,
        bin_edge = bin_edge,
        bin_fill = bin_fill,
        vline = vline,
        vline_colour = vline_colour,
        ...
    )
    
    return(out)
}


.annotation_histogram <- setClass(
    "annotation_histogram",
    contains = "chart",
    slots = c(
        factor_name = "entity",
        bins = "entity",
        bin_edge = "entity",
        bin_fill = "entity",
        vline = "entity",
        vline_colour = "entity"
    ),
    prototype = list(
        name = "Annotation histogram",
        description = paste0(
            "Display a histogram of value in the specified column of an ",
            "annotation_source."
        ),
        type = "histogram",
        .params = c(
            "factor_name", "bins", "bin_edge", "bin_fill", "vline",
            "vline_colour"
        ),
        libraries = "ggplot2",
        factor_name = entity(
            name = "Factor name",
            description = paste0(
                "The name of the column in the `annotation_source` to ",
                "generate a histogram from."
            ),
            type = "character",
            value = "V1",
            max_length = 1
        ),
        bins = entity(
            name = "Bins",
            description = paste0(
                "The number of bins to use when computing the histogram."
            ),
            value = 30,
            max_length = Inf,
            type = c("numeric", "integer")
        ),
        bin_edge = entity(
            name = "Bin edge colour",
            description = paste0(
                "The colour to use when plotting the edges of bins."
            ),
            value = "grey",
            max_length = 1,
            type = c("character")
        ),
        bin_fill = entity(
            name = "Bin fill colour",
            description = paste0(
                "The colour to use when plotting the bins."
            ),
            value = "lightgrey",
            max_length = 1,
            type = c("character")
        ),
        vline = entity(
            name = "Vertical lines",
            description = paste0(
                "The x-axis location of veritcal lines used to indicate e.g. ",
                "upper and lower limits. Use NULL if not required."
            ),
            value = NULL,
            max_length = Inf,
            type = c("numeric", "NULL", "list")
        ),
        vline_colour = entity(
            name = "Vertical line colour",
            description = paste0(
                "The colour to use when plotting vertical lines."
            ),
            value = "red",
            max_length = 1,
            type = c("character")
        )
    )
)

#' @export
setMethod(
    f = "chart_plot",
    signature = c("annotation_histogram", "annotation_source"),
    definition = function(obj, dobj) {
        # force numeric
        dobj$data[[obj$factor_name]] <-
            as.numeric(dobj$data[[obj$factor_name]])
        
        g <- ggplot(data = dobj$data) +
            geom_histogram(
                mapping = aes(x = .data[[obj$factor_name]]),
                bins = obj$bins,
                colour = obj$bin_edge,
                fill = obj$bin_fill
            )
        
        if (!is.null(obj$vline)) {
            for (k in seq_len(length(obj$vline))) {
                g <- g +
                    geom_vline(
                        xintercept = obj$vline[k],
                        colour = obj$vline_colour,
                        linewidth = 1,
                        show.legend = FALSE
                    )
            }
        }
        
        g <- g + theme_Publication(12)
        return(g)
    }
)
