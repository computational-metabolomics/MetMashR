#' @eval get_description('annotation_pie_chart')
#' @include annotation_source_class.R
#' @export
annotation_bar_chart <- function(
        factor_name,
        label_rotation = FALSE,
        label_location = "inside",
        label_type = "percent",
        legend = FALSE,
        ...) {
    out <- struct::new_struct(
        "annotation_bar_chart",
        factor_name = factor_name,
        label_rotation = label_rotation,
        label_location = label_location,
        label_type = label_type,
        legend = legend,
        ...
    )
    
    return(out)
}


.annotation_bar_chart <- setClass(
    "annotation_bar_chart",
    contains = "chart",
    slots = c(
        factor_name = "entity",
        label_rotation = "entity",
        label_location = "enum",
        label_type = "enum",
        legend = "entity"
    ),
    prototype = list(
        name = "Annotation bar chart",
        description = paste0(
            "Display a bar chart of labels in the specified column of an ",
            "annotation_source."
        ),
        type = "image",
        .params = c(
            "factor_name", "label_location", "label_rotation", "legend",
            "label_type"
        ),
        libraries = "ggplot2",
        factor_name = entity(
            name = "Factor name",
            description = paste0(
                "The name of the column in the `annotation_source` to ",
                "generate a chart from."
            ),
            type = "character",
            value = "V1",
            max_length = 1
        ),
        label_type = enum(
            name = "Label type",
            description = c(
                "percent" = paste0(
                    "Labels will include the percentage for the segment."
                ),
                "count" = "Labels will include the count for the segment.",
                "none" = "Labels will not include extra information."
            ),
            value = "percent",
            max_length = 1,
            type = "character",
            allowed = c("percent", "count", "none")
        ),
        label_rotation = entity(
            name = "Rotate labels",
            description = c(
                `TRUE` = "Rotate labels to match segments.",
                `FALSE` = "Do not rotate labels."
            ),
            type = "logical",
            value = TRUE,
            max_length = 1
        ),
        label_location = enum(
            name = "Label location",
            description = c(
                inside = "Labels are displayed inside the bars.",
                outside = "Labels are displayed outside the bars."
            ),
            type = "character",
            value = "inside",
            max_length = 1,
            allowed = c("inside", "outside")
        ),
        legend = entity(
            name = "Display legend",
            description = c(
                `TRUE` = "Groups are indicated using a legend.",
                `FALSE` = "Groups are indicated in the labels."
            ),
            type = "logical",
            value = TRUE,
            max_length = 1
        )
    )
)

#' @export
setMethod(
    f = "chart_plot",
    signature = c("annotation_bar_chart", "annotation_source"),
    definition = function(obj, dobj) {
        # counts from column of labels
        df <- dobj$data %>%
            group_by(.data[[obj$factor_name]]) %>%
            summarise(count = n()) %>%
            tidyr::complete(.data[[obj$factor_name]], fill = list(count = 0))
        
        # labels
        df$label <- ""
        if (obj$label_type == "percent") {
            df$label <- paste0(
                round(df$count / sum(df$count) * 100, digits = 1), "%"
            )
        } else if (obj$label_type == "count") {
            df$label <- as.character(df$count)
        }
        
        if (obj$label_rotation) {
            df$label <- paste0(" ", df$label, " ")
        } else {
            if (obj$label_location == "inside") {
                df$label <- paste0("\n", df$label)
            } else {
                df$label <- paste0(df$label)
            }
        }
        
        # add newlines of spaces to offset depending on rotation
        if (!obj$legend) {
            if (obj$label_type != "none") {
                df$label <- paste0(
                    df[[obj$factor_name]],
                    " (", df$label, ")"
                )
            } else {
                df$label <- df[[obj$factor_name]]
            }
        }
        
        if (!obj$label_rotation) {
            df$rotate <- 0
        } else {
            df$rotate <- -90
        }
        
        df$hjust <- 0.5
        if (obj$label_rotation) {
            if (obj$label_location == "inside") {
                df$hjust <- 0
            } else {
                df$hjust <- 1
            }
        }
        
        # plot
        g <- ggplot(
            data = df,
            aes(
                x = .data[[obj$factor_name]],
                y = .data[["count"]],
                fill = .data[[obj$factor_name]]
            )
        ) +
            geom_bar(colour = "black", stat = "identity") +
            geom_text(
                aes(
                    x = .data[[obj$factor_name]],
                    y = .data[["count"]],
                    label = .data[["label"]],
                    angle = rotate,
                    hjust = .data[["hjust"]]
                )
            ) +
            coord_cartesian(clip = "off") +
            structToolbox:::theme_Publication(12) +
            scale_fill_Publication() +
            theme_Publication()
        
        # legend
        if (!obj$legend) {
            g <- g + theme(legend.position = "none")
        } else {
            g <- g + theme(axis.text.x = element_blank())
        }
        
        
        return(g)
    }
)
