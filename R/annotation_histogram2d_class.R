#' @eval get_description('annotation_histogram2d')
#' @include annotation_histogram_class.R
#' @export
annotation_histogram2d <- function(factor_name,
    bins = 30,
    bin_edge = "grey",
    bin_fill = "lightgrey",
    vline = NULL,
    vline_colour = "red",
    ...) {
    out <- struct::new_struct(
        "annotation_histogram2d",
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


.annotation_histogram2d <- setClass(
    "annotation_histogram2d",
    contains = "annotation_histogram",
    slots = c(
        factor_name = "entity",
        bins = "entity",
        bin_edge = "entity",
        bin_fill = "entity",
        vline = "entity",
        vline_colour = "entity"
    ),
    prototype = list(
        name = "Annotation 2D histogram",
        description = paste0(
            "Display a histogram of value in the specified columns of an ",
            "annotation_source."
        ),
        type = "histogram",
        .params = c(
            "factor_name", "bins", "bin_edge", "bin_fill", "vline",
            "vline_colour"
        ),
        libraries = c("ggplot2", "patchwork"),
        factor_name = entity(
            name = "Factor name",
            description = paste0(
                "The names of the two columns in the `annotation_source` to ",
                "generate histograms from."
            ),
            type = "character",
            value = "V1",
            max_length = 2
        ),
        bins = entity(
            name = "Bins",
            description = paste0(
                "The number of bins to use when computing the histograms."
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
                "The x-axis location of lines used to indicate e.g. ",
                "upper and lower limits. Use NULL if not required. A 2 ",
                "element ",
                "list can be provided to set vlines for each `factor_name`."
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
#' @template chart_plot
setMethod(
    f = "chart_plot",
    signature = c("annotation_histogram2d", "annotation_source"),
    definition = function(obj, dobj) {
        # update vline if not a list
        if (!is.null(obj$vline)) {
            if (!is.list(obj$vline)) {
                obj$vline <- list(
                    obj$vline,
                    obj$vline
                )
            }
        }

        # 2d histogram
        g1 <- ggplot(
            data = dobj$data,
            aes(
                x = as.numeric(.data[[obj$factor_name[1]]]),
                y = as.numeric(.data[[obj$factor_name[2]]])
            )
        ) +
            geom_bin2d(bins = obj$bins) +
            theme_Publication(12) +
            theme(
                legend.position = "none",
                plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
                panel.border = element_rect(fill = NA, colour = "black")
            ) +
            xlab(obj$factor_name[1]) +
            ylab(obj$factor_name[2])

        # 1d histograms
        C <- annotation_histogram(
            factor_name = obj$factor_name[1],
            bins = obj$bins,
            vline = obj$vline[[1]]
        )

        g2 <- chart_plot(C, dobj) +
            xlab(element_blank()) +
            theme(
                legend.position = "none",
                plot.margin = unit(c(0, 0, 0, 0), "cm"),
                panel.border = element_rect(fill = NA, colour = "black"),
                axis.text.x = element_blank()
            )


        C <- annotation_histogram(
            factor_name = obj$factor_name[2],
            bins = obj$bins,
            vline = obj$vline[[2]]
        )

        g3 <- chart_plot(C, dobj) +
            xlab(element_blank()) +
            theme(
                legend.position = "none",
                plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
                panel.border = element_rect(fill = NA, colour = "black"),
                axis.text.y = element_blank()
            )

        # build plots, get x and y lims
        xlims <- .get_limits(g1, "x")
        xlims <- rbind(xlims, .get_limits(g2, "x"))

        ylims <- .get_limits(g1, "y")
        ylims <- rbind(ylims, .get_limits(g3, "x"))

        g <- wrap_plots(
            g2 + coord_cartesian(xlim = (c(min(xlims$lo), max(xlims$hi)))),
            plot_spacer(),
            g1 + coord_cartesian(
                xlim = c(min(xlims$lo), max(xlims$hi)),
                ylim = c(min(ylims$lo), max(ylims$hi))
            ),
            g3 + coord_flip(xlim = c(min(ylims$lo), max(ylims$hi))),
            ncol = 2,
            widths = c(80, 20),
            heights = c(20, 80)
        )
        return(g)
    }
)

# internal function to get axes limits from a ggplot
.get_limits <- function(g, a) {
    g <- ggplot_build(g)
    if (a == "x") {
        lims <- data.frame(
            lo = g$layout$panel_params[[1]]$x.range[1],
            hi = g$layout$panel_params[[1]]$x.range[2]
        )
    } else {
        lims <- data.frame(
            lo = g$layout$panel_params[[1]]$y.range[1],
            hi = g$layout$panel_params[[1]]$y.range[2]
        )
    }
    return(lims)
}
