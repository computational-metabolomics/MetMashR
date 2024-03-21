#' @eval get_description('annotation_venn_chart')
#' @include annotation_source_class.R
#'
#' @export
annotation_venn_chart <- function(factor_name,
                                  group_column = NULL,
                                  fill_colour = "white",
                                  line_colour = "black",
                                  labels = TRUE,
                                  legend = FALSE,
                                  ...) {
    out <- struct::new_struct(
        "annotation_venn_chart",
        factor_name = factor_name,
        group_column = group_column,
        fill_colour = fill_colour,
        line_colour = line_colour,
        labels = labels,
        legend = legend,
        ...
    )

    return(out)
}


.annotation_venn_chart <- setClass(
    "annotation_venn_chart",
    contains = "chart",
    slots = c(
        factor_name = "entity",
        group_column = "entity",
        fill_colour = "entity",
        line_colour = "entity",
        labels = "entity",
        legend = "entity"
    ),
    prototype = list(
        name = "Annotation venn chart",
        description = paste0(
            "Display a venn diagram of labels present in two ",
            "annotation_sources."
        ),
        type = "image",
        .params = c(
            "factor_name", "line_colour", "fill_colour", "labels", "legend",
            "group_column"
        ),
        libraries = c("RVenn", "ggVennDiagram"),
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
                "ignored if there are multiple input tables, as each table is ",
                "considered to be a group. This parameter is also ignored if ",
                "more than one `factor_name` is provided, as each column is ",
                "considered a group."
            ),
            type = c("character", "NULL"),
            value = NULL,
            max_length = 1
        ),
        line_colour = entity(
            name = "Line colour",
            description = paste0(
                "The line colour of the groups in a format compatible with ",
                'ggplot e.g. "black" or "#000000". Special case ".group" ',
                'sets the colour based on the group label, and ".none" will ',
                "not display lines."
            ),
            type = "character",
            value = ".group",
            max_length = 1
        ),
        fill_colour = entity(
            name = "Fill colour",
            description = paste0(
                "The line colour of the groups in a format compatible with ",
                'ggplot e.g. "black" or "#000000". Special case ".group" ',
                'sets the colour based on the group label and "none" will ',
                "not fill the groups."
            ),
            type = "character",
            value = ".group",
            max_length = 1
        ),
        labels = entity(
            name = "Group labels",
            description = c(
                "TRUE" = "Include group labels on the plot.",
                "FALSE" = "Do not inlude group labels on the plot."
            ),
            type = "logical",
            value = FALSE,
            max_length = 1
        ),
        legend = entity(
            name = "Legend",
            description = c(
                "TRUE" = "Include a legend in the plot.",
                "FALSE" = "Do not inlude a legend in the plot."
            ),
            type = "logical",
            value = FALSE,
            max_length = 1
        )
    )
)

#' @export
setMethod(
    f = "chart_plot",
    signature = c("annotation_venn_chart", "annotation_source"),
    definition = function(obj, dobj, ...) {
        # check for dots
        L <- list(...)

        # if we got more than one table...
        if (length(L) > 0) {
            # gather all annotation_sources
            L <- c(list(dobj), L)

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
                obj$factor_name <- obj$factor_name[1]
            }

            # get tags
            tags <- lapply(L, param_value, name = "tag")
            names(L) <- tags

            # get tables
            L <- lapply(L, param_value, name = "data")

            # get columns
            L <- mapply("[[", L, obj$factor_name)
        } else if (length(obj$factor_name) > 1) {
            # comparing multiple columns
            L <- as.list(dobj$data[obj$factor_name])
        } else {
            # if we only got one table and one factor...
            u <- unique(dobj$data[[obj$group_column]])

            # construct list for Venn
            L <- list()
            for (k in u) {
                this <- dobj$data[[obj$factor_name]]
                L[[k]] <- this[dobj$data[[obj$group_column]] == k]
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

        # plot
        g <- venn_this(obj, L)
        return(g)
    }
)

#' @export
setMethod(
    f = "chart_plot",
    signature = c("annotation_venn_chart", "list"),
    definition = function(obj, dobj) {
        L <- c(list(obj), dobj)
        names(L)[1] <- "obj"
        names(L)[2] <- "dobj"
        names(L)[3] <- "gobj"
        g <- do.call(chart_plot, L)
        return(g)
    }
)

# internal function ot plot venn
venn_this <- function(obj, L) {
    shape <- NULL
    if (length(L) == 2) {
        shape <- "201"
    }

    # venn
    this <- ggVennDiagram::process_data(RVenn::Venn(L), shape_id = shape)

    # ellipse
    vE <- ggVennDiagram::venn_setedge(this)

    # plot params
    Sfill <- list(
        data = vE,
        alpha = 1 / length(L),
        colour = NA,
        show.legend = TRUE,
        mapping = aes(x = X, y = Y, group = id)
    )
    Sline <- list(
        data = vE,
        linewidth = 0.5,
        show.legend = FALSE,
        mapping = aes(x = X, y = Y, group = id)
    )

    # colours
    if (obj$fill_colour == ".group") {
        Sfill$mapping <- aes(x = X, y = Y, group = id, fill = .data[["id"]])
    } else if (obj$fill_colour == ".none") {
        Sfill$fill <- NA
    } else {
        Sfill$fill <- obj$fill_colour
    }
    if (obj$line_colour == ".group") {
        Sline$mapping <- aes(colour = .data[["id"]])
    } else if (obj$line_colour == ".none") {
        Sline$colour <- NA
    } else {
        Sline$colour <- obj$line_colour
    }

    # plot
    g <- ggplot() +
        # fill layer
        do.call(geom_polygon, Sfill)

    g <- g +
        # line layer
        do.call(geom_path, Sline)

    # region labels
    g <- g +
        geom_text(aes(x = X, y = Y, label = .data[["count"]]),
            data = ggVennDiagram::venn_regionlabel(this)
        )

    if (obj$labels) {
        # set labels
        g <- g +
            geom_text(aes(x = X, y = Y, label = .data[["name"]]),
                data = ggVennDiagram::venn_setlabel(this)
            )
    }

    vsl <- ggVennDiagram::venn_setlabel(this)
    # theme
    g <- g +
        theme_void() +
        scale_x_continuous(expand = expansion(mult = 0.2)) +
        scale_color_Publication(name = "Group", labels = vsl$name) +
        scale_fill_Publication(name = "Group", labels = vsl$name)

    if (!obj$legend) {
        g <- g + theme(legend.position = "none")
    } else {
        g <- g + theme(legend.position = "right")
    }
    g <- g + coord_fixed()

    return(g)
}
