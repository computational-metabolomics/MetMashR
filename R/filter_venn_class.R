#' @eval get_description('filter_venn')
#' @export
#' @include annotation_source_class.R
filter_venn <- function(factor_name,
    group_column = NULL,
    tables = NULL,
    filter = NULL,
    mode = "include",
    ...) {
    out <- struct::new_struct(
        "filter_venn",
        factor_name = factor_name,
        group_column = group_column,
        tables = tables,
        filter = filter,
        mode = mode,
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
        filter = "entity",
        mode = "enum",
        filtered = "entity",
        flags = "entity"
    ),
    prototype = list(
        name = "Filter by factor levels",
        description = paste0(
            "Removes (or includes) annotations such that the named column ",
            "excludes (or includes) the specified intersection levels. ",
            "Supports any number of groups using intersection-based filtering. ",
            "If no levels are specified, all available intersection levels ",
            "will be returned for inspection. If invalid levels are specified, ",
            "a warning will be shown with the list of valid levels."
        ),
        type = "univariate",
        predicted = "filtered",
        .params = c(
            "factor_name", "group_column", "tables", "filter", "mode"
        ),
        .outputs = c("filtered", "flags"),
        factor_name = entity(
            name = "Factor name",
            description = paste0(
                "The name of the column(s) in the `annotation_source` to ",
                "generate intersection groups from. Supports any number of ",
                "columns for intersection-based filtering."
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
        filter = entity(
            name = "Intersection filter",
            description = paste0(
                "A function to filter intersections based on their properties. ",
                "The function should take region_data as input and return a logical ",
                "vector indicating which intersections to keep. ",
                "Use upset_intersections(), upset_min_size(), upset_min_groups(), ",
                "upset_max_groups(), or create custom filter functions."
            ),
            value = NULL,
            type = c("function", "NULL"),
            max_length = 1
        ),
        mode = enum(
            name = "Filter mode",
            description = c(
                "include" = paste0(
                    "Only items that appear in the filtered intersections ",
                    "are kept in the output."
                ),
                "exclude" = paste0(
                    "Items that appear in the filtered intersections ",
                    "are removed from the output."
                )
            ),
            type = c("character"),
            value = "include",
            max_length = 1,
            allowed = c("include", "exclude")
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
        )
    )
)


#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("filter_venn", "annotation_source"),
    definition = function(M, D) {
        # tables
        L <- M$tables
        L <- process_venn_dots(L, D, M)

        # create Venn object and get region data
        venn_obj <- ggVennDiagram::Venn(L)
        region_data <- ggVennDiagram::process_region_data(venn_obj, sep = "/", specific = TRUE)

        # apply filter function
        if (is.function(M$filter)) {
            valid_regions <- M$filter(region_data)
        } else {
            valid_regions <- rep(TRUE, nrow(region_data))
        }

        # get all items that appear in valid intersections
        valid_items <- character(0)
        for (j in which(valid_regions)) {
            region_items <- region_data$item[[j]]
            if (length(region_items) > 0) {
                valid_items <- c(valid_items, region_items)
            }
        }
        valid_items <- unique(valid_items)
        
        # filter the main input D based on mode
        if (M$mode == "include") {
            keep_rows <- D$data[[M$factor_name[1]]] %in% valid_items
        } else {
            keep_rows <- !(D$data[[M$factor_name[1]]] %in% valid_items)
        }
        D2 <- D
        D2$data <- D2$data[keep_rows, , drop = FALSE]
        
        # create flags
        flags <- data.frame(
            original_index = which(keep_rows),
            region = D$data[[M$factor_name[1]]][keep_rows],
            stringsAsFactors = FALSE
        )
        
        M$filtered <- D2
        
        M$flags <- flags
        return(M)
    }
)
