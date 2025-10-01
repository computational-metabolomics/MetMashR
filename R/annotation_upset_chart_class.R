#' @eval get_description('annotation_upset_chart')
#' @include annotation_venn_chart.R
#' @export
#' @details
#' The plot
#' object returned is of class 'aplot' which may not be compatible with all plot
#' combination functions. To combine with other ggplot objects using cowplot or 
#' patchwork, use ggplotify::as.ggplot() to convert the plot object:
#' 
#' \preformatted{
#' library(ggplotify)
#' g <- chart_plot(C, data)
#' g_ggplot <- as.ggplot(g)
#' cowplot::plot_grid(g1, g_ggplot, nrow = 1)
#' }
#' 
#' @section Filtering:
#' Use the `filter` parameter to filter intersections based on their properties:
#' 
#' \preformatted{
#' # Filter by minimum size
#' C <- annotation_upset_chart(factor_name = "V1", filter = upset_min_size(5))
#' 
#' # Filter by minimum number of groups
#' C <- annotation_upset_chart(factor_name = "V1", filter = upset_min_groups(3))
#' 
#' # Filter to show only specific combinations
#' C <- annotation_upset_chart(factor_name = "V1", 
#'                           filter = upset_intersections(c("A/B", "B/C")))
#' 
#' # Custom filter function
#' custom_filter <- function(region_data) {
#'   region_data$count >= 3 & grepl("A", region_data$name)
#' }
#' C <- annotation_upset_chart(factor_name = "V1", filter = custom_filter)
#' }
#' 
#' @note
#' The interface to this class has changed. Some parameters have been renamed:
#' - 'width_ratio' -> 'relative_width'
#' - 'xlabel' -> 'top_bar_y_label' 
#' - 'sort_intersections' -> 'order_intersect_by'
#' - 'intersections' -> 'nintersects'
#' - 'n_intersections' -> 'nintersects'
#' - 'queries' -> (removed)
#' - 'keep_empty_group' -> (removed)
#' - 'sort_sets' -> 'order_set_by'
#' 
#' Old parameter names will trigger deprecation warnings.
annotation_upset_chart <- function(
    factor_name,
    group_column = NULL,
    order_intersect_by = "size",
    order_set_by = "name",
    nintersects = NULL,
    filter = NULL,
    relative_width = 0.3,
    relative_height = 3,
    top_bar_color = "grey30",
    top_bar_y_label = NULL,
    top_bar_show_numbers = TRUE,
    top_bar_numbers_size = 3,
    sets_bar_color = "grey30",
    sets_bar_show_numbers = FALSE,
    sets_bar_x_label = "Set Size",
    sets_bar_position = "left",
    intersection_matrix_color = "grey30",
    specific = TRUE,
    ...) {
    
    # check for old usage
    dots = list(...)
    old_slots=c(
        'width_ratio',
        'xlabel',
        'min_size',
        'sort_intersections',
        'intersections',
        'n_intersections',
        'queries',
        'keep_empty_group',
        'sort_sets'
    )
    used_old = intersect(names(dots),old_slots)
    check = length(used_old)>0
    
    if (check){
        .Deprecated('annotation_upset_chart (new signature)',
                    msg = paste0(
                        'The interface to this class has changed. See',
                        '?annotation_upset_chart.'
                    )
        )
        # exclude deprecated
        dots[used_old] <- NULL
    }

    # create parameter list
    params <- list(
        factor_name = factor_name,
        group_column = group_column,
        order_intersect_by = order_intersect_by,
        order_set_by = order_set_by,
        nintersects = nintersects,
        filter = filter,
        relative_width = relative_width,
        relative_height = relative_height,
        top_bar_color = top_bar_color,
        top_bar_y_label = top_bar_y_label,
        top_bar_show_numbers = top_bar_show_numbers,
        top_bar_numbers_size = top_bar_numbers_size,
        sets_bar_color = sets_bar_color,
        sets_bar_show_numbers = sets_bar_show_numbers,
        sets_bar_x_label = sets_bar_x_label,
        sets_bar_position = sets_bar_position,
        intersection_matrix_color = intersection_matrix_color,
        specific = specific
    )
    
    # add dots if present
    if (length(dots) > 0) {
        params <- c(params, dots)
    }
    
    # create struct object
    out <- do.call(struct::new_struct, c("annotation_upset_chart", params))
    
    return(out)
}


.annotation_upset_chart <- setClass(
    "annotation_upset_chart",
    contains = "chart",
    slots = c(
        factor_name = "entity",
        group_column = "entity",
        order_intersect_by = "entity",
        order_set_by = "entity",
        nintersects = "entity",
        filter = "entity",
        relative_width = "entity",
        relative_height = "entity",
        top_bar_color = "entity",
        top_bar_y_label = "entity",
        top_bar_show_numbers = "entity",
        top_bar_numbers_size = "entity",
        sets_bar_color = "entity",
        sets_bar_show_numbers = "entity",
        sets_bar_x_label = "entity",
        sets_bar_position = "entity",
        intersection_matrix_color = "entity",
        specific = "entity"
    ),
    prototype = list(
        name = "Annotation UpSet chart",
        description = paste0(
            "Display an UpSet chart of labels in the specified column of an ",
            "annotation_source."
        ),
        type = "image",
        .params = c(
            "factor_name", "group_column", "order_intersect_by", "order_set_by",
            "nintersects", "filter", "relative_width", "relative_height", "top_bar_color",
            "top_bar_y_label", "top_bar_show_numbers", "top_bar_numbers_size",
            "sets_bar_color", "sets_bar_show_numbers", "sets_bar_x_label",
            "sets_bar_position", "intersection_matrix_color", "specific"
        ),
        libraries = "ggVennDiagram",
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
        order_intersect_by = enum(
            name = "Order intersect by",
            description = c(
                "size" = "Intersections are sorted by size (largest first).",
                "name" = "Intersections are sorted by name alphabetically.",
                "none" = "Intersections are not sorted"
            ),
            type = "character",
            value = "size",
            max_length = 1,
            allowed = c("size", "name", "none")
        ),
        order_set_by = enum(
            name = "Order set by",
            description = c(
                "size" = "Sets are sorted by size (largest first).",
                "name" = "Sets are sorted by name alphabetically.",
                "none" = "Sets are not sorted"
            ),
            type = "character",
            value = "name",
            max_length = 1,
            allowed = c("size", "name", "none")
        ),
        nintersects = entity(
            name = "Number of intersections",
            description = paste0(
                "The number of intersections to include in the plot."
            ),
            value = NULL,
            type = c("numeric", "integer", "NULL"),
            max_length = 1
        ),
        filter = entity(
            name = "Intersection filter",
            description = paste0(
                "A function or list of functions to filter intersections based on ",
                "their properties. The function(s) should take region_data as input ",
                "and return a logical vector indicating which intersections to keep. ",
                "Use upset_min_size(), upset_min_groups(), upset_max_groups(), ",
                "upset_intersections(), or create custom filter functions."
            ),
            value = NULL,
            type = c("function", "NULL"),
            max_length = Inf
        ),
        relative_width = entity(
            name = "Relative width",
            description = paste0(
                "The relative width of the left panel in the upset plot."
            ),
            value = 0.3,
            type = "numeric",
            max_length = 1
        ),
        relative_height = entity(
            name = "Relative height",
            description = paste0(
                "The relative height of the top panel in the upset plot."
            ),
            value = 3,
            type = "numeric",
            max_length = 1
        ),
        top_bar_color = entity(
            name = "Top bar color",
            description = paste0(
                "The color of the top bar chart showing intersection sizes."
            ),
            value = "grey30",
            type = "character",
            max_length = 1
        ),
        top_bar_y_label = entity(
            name = "Top bar Y label",
            description = paste0(
                "The label for the Y-axis of the top bar chart."
            ),
            value = NULL,
            type = c("character", "NULL"),
            max_length = 1
        ),
        top_bar_show_numbers = entity(
            name = "Show top bar numbers",
            description = paste0(
                "Whether to show numbers on the top bar chart."
            ),
            value = TRUE,
            type = "logical",
            max_length = 1
        ),
        top_bar_numbers_size = entity(
            name = "Top bar numbers size",
            description = paste0(
                "The text size of numbers on the top bar chart."
            ),
            value = 3,
            type = "numeric",
            max_length = 1
        ),
        sets_bar_color = entity(
            name = "Sets bar color",
            description = paste0(
                "The color of the sets bar chart."
            ),
            value = "grey30",
            type = "character",
            max_length = 1
        ),
        sets_bar_show_numbers = entity(
            name = "Show sets bar numbers",
            description = paste0(
                "Whether to show numbers on the sets bar chart."
            ),
            value = FALSE,
            type = "logical",
            max_length = 1
        ),
        sets_bar_x_label = entity(
            name = "Sets bar X label",
            description = paste0(
                "The label for the X-axis of the sets bar chart."
            ),
            value = "Set Size",
            type = "character",
            max_length = 1
        ),
        sets_bar_position = enum(
            name = "Sets bar position",
            description = c(
                "left" = "Position the sets bar chart on the left side.",
                "right" = "Position the sets bar chart on the right side."
            ),
            type = "character",
            value = "left",
            max_length = 1,
            allowed = c("left", "right")
        ),
        intersection_matrix_color = entity(
            name = "Intersection matrix color",
            description = paste0(
                "The color of the intersection matrix dots and lines."
            ),
            value = "grey30",
            type = "character",
            max_length = 1
        ),
        specific = entity(
            name = "Specific items only",
            description = paste0(
                "Whether to include only specific items in subsets (TRUE) ",
                "or all overlapping items (FALSE)."
            ),
            value = TRUE,
            type = "logical",
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

        # handle multiple inputs
        L = list(...)
        L = process_venn_dots(L, dobj, obj)

        # create Venn object for ggVennDiagram
        venn_obj <- ggVennDiagram::Venn(L)
        
        # apply filters if specified
        if (!is.null(obj$filter)) {
            # get region data for filtering
            region_data <- ggVennDiagram::process_region_data(venn_obj, sep = "/", specific = obj$specific)
            
            # apply filter function
            if (is.function(obj$filter)) {
                valid_regions <- obj$filter(region_data)
            } else {
                valid_regions <- rep(TRUE, nrow(region_data))
            }
            
            if (any(valid_regions)) {
                # get all items that appear in valid intersections
                valid_items <- character(0)
                for (j in which(valid_regions)) {
                    region_items <- region_data$item[[j]]
                    if (length(region_items) > 0) {
                        valid_items <- c(valid_items, region_items)
                    }
                }
                valid_items <- unique(valid_items)
                
                # filter each set to only include items that appear in valid intersections
                filtered_L <- list()
                for (i in seq_along(L)) {
                    set_items <- L[[i]]
                    filtered_L[[i]] <- intersect(set_items, valid_items)
                }
                names(filtered_L) <- names(L)
                
                # create new Venn object with filtered data
                venn_obj <- ggVennDiagram::Venn(filtered_L)
            } else {
                # if no valid intersections, create empty Venn object
                empty_L <- lapply(L, function(x) character(0))
                names(empty_L) <- names(L)
                venn_obj <- ggVennDiagram::Venn(empty_L)
            }
        }
        
        # create the plot
        g <- ggVennDiagram::plot_upset(
            venn = venn_obj,
            nintersects = obj$nintersects,
            `order.intersect.by` = obj$order_intersect_by,
            `order.set.by` = obj$order_set_by,
            relative_width = obj$relative_width,
            relative_height = obj$relative_height,
            `top.bar.color` = obj$top_bar_color,
            `top.bar.y.label` = obj$top_bar_y_label,
            `top.bar.show.numbers` = obj$top_bar_show_numbers,
            `top.bar.numbers.size` = obj$top_bar_numbers_size,
            `sets.bar.color` = obj$sets_bar_color,
            `sets.bar.show.numbers` = obj$sets_bar_show_numbers,
            `sets.bar.x.label` = obj$sets_bar_x_label,
            `sets.bar.position` = obj$sets_bar_position,
            `intersection.matrix.color` = obj$intersection_matrix_color,
            specific = obj$specific
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

# Filter functions for intersection filtering
#' @export
upset_min_size <- function(min_size) {
    function(region_data) {
        region_data$count >= min_size
    }
}

#' @export
upset_min_groups <- function(min_groups) {
    function(region_data) {
        group_counts <- sapply(region_data$name, function(x) {
            if (x == "") return(0)
            groups <- strsplit(x, "/")[[1]]
            length(groups)
        })
        group_counts >= min_groups
    }
}

#' @export
upset_max_groups <- function(max_groups) {
    function(region_data) {
        group_counts <- sapply(region_data$name, function(x) {
            if (x == "") return(0)
            groups <- strsplit(x, "/")[[1]]
            length(groups)
        })
        group_counts <= max_groups
    }
}

#' @export
upset_intersections <- function(combinations) {
    function(region_data) {
        # Normalize the input combinations (sort groups alphabetically for consistency)
        normalized_combinations <- sapply(combinations, function(combo) {
            groups <- strsplit(combo, "/")[[1]]
            paste(sort(groups), collapse = "/")
        })
        
        # Normalize the region names for comparison
        normalized_regions <- sapply(region_data$name, function(name) {
            if (name == "") return("")
            groups <- strsplit(name, "/")[[1]]
            paste(sort(groups), collapse = "/")
        })
        
        # Check which regions match the specified combinations
        normalized_regions %in% normalized_combinations
    }
}

#' UpSet chart filter helper functions
#' 
#' These functions create filters for the `annotation_upset_chart` class to 
#' control which intersections are displayed in UpSet plots. Each function 
#' returns a filter function that can be used with the `filter` parameter.
#' 
#' @param min_size `numeric` The minimum number of items in an intersection
#' @param min_groups `numeric` The minimum number of groups in an intersection  
#' @param max_groups `numeric` The maximum number of groups in an intersection
#' @param combinations `character` Vector of specific intersection combinations to include (e.g., c("A/B", "B/C"))
#' 
#' @return A function that takes `region_data` as input and returns a logical 
#' vector indicating which intersections to keep.
#' 
#' @details
#' These filter functions work by analyzing the region data from the Venn 
#' diagram to determine which intersections meet the specified criteria:
#' 
#' - `upset_min_size()`: Filters intersections based on the number of items
#' - `upset_min_groups()`: Filters intersections based on the minimum number 
#'   of groups involved
#' - `upset_max_groups()`: Filters intersections based on the maximum number 
#'   of groups involved
#' - `upset_intersections()`: Filters to show only specific intersection 
#'   combinations (e.g., "A/B", "B/C", "A/B/C")
#' 
#' For complex filtering logic, create custom filter functions:
#' 
#' \preformatted{
#' # Single filter
#' filter = upset_min_size(5)
#' 
#' # Specific combinations only
#' filter = upset_intersections(c("A/B", "B/C"))
#' 
#' # Custom filter function with AND logic
#' custom_filter <- function(region_data) {
#'   region_data$count >= 3 & region_data$count <= 10 & grepl("A", region_data$name)
#' }
#' 
#' # Custom filter function with OR logic
#' or_filter <- function(region_data) {
#'   region_data$count >= 5 | grepl("B/C", region_data$name)
#' }
#' }
#' 
#' @examples
#' \dontrun{
#' # Filter to show only intersections with 5+ items
#' C <- annotation_upset_chart(factor_name = "V1", filter = upset_min_size(5))
#' 
#' # Filter to show only intersections involving 3+ groups
#' C <- annotation_upset_chart(factor_name = "V1", filter = upset_min_groups(3))
#' 
#' # Filter to show only intersections involving 2-4 groups (custom function)
#' group_range_filter <- function(region_data) {
#'   group_counts <- sapply(region_data$name, function(x) {
#'     if (x == "") return(0)
#'     groups <- strsplit(x, "/")[[1]]
#'     length(groups)
#'   })
#'   group_counts >= 2 & group_counts <= 4
#' }
#' C <- annotation_upset_chart(factor_name = "V1", filter = group_range_filter)
#' 
#' # Filter to show only specific combinations
#' C <- annotation_upset_chart(factor_name = "V1", 
#'                           filter = upset_intersections(c("A/B", "B/C")))
#' 
#' # Custom filter combining size and group criteria
#' size_and_group_filter <- function(region_data) {
#'   region_data$count >= 3 & sapply(region_data$name, function(x) {
#'     if (x == "") return(FALSE)
#'     groups <- strsplit(x, "/")[[1]]
#'     length(groups) >= 2
#'   })
#' }
#' C <- annotation_upset_chart(factor_name = "V1", filter = size_and_group_filter)
#' }
#' 
#' @name upset_filters
NULL
