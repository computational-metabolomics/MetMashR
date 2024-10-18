#' @importFrom utils capture.output
#' @importFrom scales manual_pal

scale_fill_Publication <- function(...) {
    # library(scales)
    discrete_scale("fill", "Publication", scales::manual_pal(
        values = c(
            "#1f78b4", "#e31a1c", "#33a02c", "#ff7f00", "#6a3d9a", "#b15928",
            "#a6cee3", "#fb9a99", "#b2df8a", "#fdbf6f", "#cab2d6", "#ffff99"
        )
    ), ...)
}

scale_color_Publication <- function(...) {
    # library(scales)
    discrete_scale("color", "Publication", scales::manual_pal(
        values = c(
            "#1f78b4", "#e31a1c", "#33a02c", "#ff7f00", "#6a3d9a", "#b15928",
            "#a6cee3", "#fb9a99", "#b2df8a", "#fdbf6f", "#cab2d6", "#ffff99"
        )
    ), ...)
}

#' @import ggthemes
theme_Publication <- function(base_size = 14) { # , base_family="helvetica") {
    (theme_foundation(base_size = base_size) + # , base_family=base_family)
        theme(
            plot.title = element_text(
                face = "bold",
                size = rel(1.2), hjust = 0.5
            ),
            text = element_text(),
            panel.background = element_rect(colour = "#ffffff"),
            plot.background = element_rect(colour = "#ffffff"),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold", size = rel(1)),
            axis.title.y = element_text(angle = 90, vjust = 2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(),
            axis.line = element_line(colour = "black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour = "#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = "#ffffff"),
            legend.key.size = unit(0.75, "cm"),
            legend.spacing = unit(0.2, "cm"),
            legend.title = element_text(face = "italic"),
            plot.margin = unit(c(10, 5, 5, 5), "mm"),
            strip.background = element_rect(
                colour = "#f0f0f0",
                fill = "#f0f0f0"
            ),
            strip.text = element_text(face = "bold")
        ))
}


# override default entity value without need to provide name, description etc
# again
.set_entity_value <- function(obj,
    param_id,
    ...) {
    # create obj
    obj <- new_struct(obj)
    # get entity
    E <- param_obj(obj, param_id)
    # set values
    L <- list(...)
    for (k in names(L)) {
        slot(E, k) <- L[[k]]
    }
    # return entity
    return(E)
}

get_description <- function(id) {
    str <- struct::get_description(id)
    str <- gsub("[a annotation_source]", "annotation_source()", str, fixed = TRUE)
    str <- gsub("[a quosures]", "wherever(A>10)", str, fixed = TRUE)
    str <- gsub("data.frame(id=NA)", "data.frame()", str, fixed = TRUE)

    str <- gsub(">>", "->", str)
    str <- gsub("M = ", "M <- ", str)
    str <- gsub("      ", "        ", str)
    w <- which(grepl("M <- ", str))
    if (length(w) > 0) {
        str <- c(strwrap(str[1:(w - 1)], 70), str[w:length(str)])
    }

    return(str)
}

utils::globalVariables(c(
    "Checked", "orange_id", "setNames", # read_cd_compounds_file
    "A", # filter_records
    "X", "Y", # venn_this
    "rotate", "wrap_plots", "plot_spacer", # annotation_histogram2d
    "compoundVariables",
    "orig_db", "blue_id", "Ion", "Name", "Formula", "mzCloud.Best.Match",
    "Charge", "Area"
))
