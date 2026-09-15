#' @eval get_description('rt_match')
#' @export
#' @include annotation_table_class.R
rt_match <- function(
        variable_meta,
        rt_column,
        rt_window,
        id_column,
        ...) {
    # make rt_window length 2
    if (length(rt_window) == 1) {
        rt_window <- c("variable_meta" = rt_window, "annotations" = rt_window)
    }

    # check rt window is named if length == 2 in case user-provided
    if (!all(names(rt_window) %in% c("variable_meta", "annotations")) |
        is.null(names(rt_window))) {
        stop(
            "If providing two retention time windows then the vector must be ",
            'named e.g. c("variable_meta" = 5, "annotations"= 2)'
        )
    }

    out <- struct::new_struct("rt_match",
        variable_meta = variable_meta,
        rt_column = rt_column,
        rt_window = rt_window,
        id_column = id_column,
        ...
    )
    return(out)
}



.rt_match <- setClass(
    "rt_match",
    contains = c("model"),
    slots = c(
        updated = "entity",
        variable_meta = "entity",
        rt_column = "entity",
        rt_window = "entity",
        id_column = "entity"
    ),
    prototype = list(
        name = "rt matching",
        description = paste0(
            "Annotations will be matched to the measured variable meta ",
            "data.frame by determining which annotations rt window overlaps ",
            "with the rt ",
            "window from the measured rt."
        ),
        type = "univariate",
        predicted = "updated",
        .params = c("variable_meta", "rt_column", "rt_window", "id_column"),
        .outputs = c("updated"),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The input annotation source with the newly generated column."
            ),
            type = "annotation_table"
        ),
        variable_meta = entity(
            name = "Variable meta data",
            description = paste0(
                "A data.frame of variable IDs and their corresponding rt ",
                "values."
            ),
            type = "data.frame"
        ),
        rt_column = entity(
            name = "rt column name",
            description = "column name of the rt values in variable_meta.",
            type = "character"
        ),
        rt_window = entity(
            name = "rt window",
            description = paste0(
                "rt window to use for matching. If a single value ",
                "is provided then the same rt is used for both variable ",
                "meta and ",
                "the annotations. A named vector can also be provided ",
                'e.g. c("variable_meta"=5,"annotations"=2) to use different ",
                "windows ',
                "for each data table."
            ),
            type = c("numeric", "integer"),
            max_length = 2,
            value = 20
        ),
        id_column = entity(
            name = "id column name",
            description = paste0(
                'column name of the variable ids in variable_meta. ",
                "id_column="rownames" will use the rownames as ids.'
            ),
            type = "character"
        )
    )
)


#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("rt_match", "annotation_table"),
    definition = function(M, D) {
        VM <- M$variable_meta

        # ensure numeric
        VM[[M$rt_column]] <- as.numeric(VM[[M$rt_column]])

        # use rownames for id if requested
        if (M$id_column == "rownames") {
            VM$.id <- rownames(VM)
        } else {
            VM$.id <- VM[[M$id_column]]
        }

        AN <- D$data

        # ensure numeric
        AN[[D$rt_column]] <- as.numeric(AN[[D$rt_column]])


        # calculate rt window for variable_meta
        VM$.rt_min <- M$variable_meta[[M$rt_column]] -
            M$rt_window[["variable_meta"]]
        VM$.rt_max <- M$variable_meta[[M$rt_column]] +
            M$rt_window[["variable_meta"]]

        # calculate rt window for annotations
        AN$.rt_min <- AN[[D$rt_column]] - M$rt_window[["annotations"]]
        AN$.rt_max <- AN[[D$rt_column]] + M$rt_window[["annotations"]]

        # see .interval_overlap_join() in mz_match_class.R -- avoids the
        # per-row which()/rep()/rbind() scan, which materialized huge
        # intermediate tables (and could exhaust memory) once variable_meta
        # reaches production scale (tens of thousands of rows)
        OUT <- .interval_overlap_join(
            VM = VM,
            AN = AN,
            vm_value_column = M$rt_column,
            an_value_column = D$rt_column,
            vm_min_column = ".rt_min",
            vm_max_column = ".rt_max",
            an_min_column = ".rt_min",
            an_max_column = ".rt_max",
            match_id_name = "rt_match_id",
            match_value_name = "rt_match",
            match_diff_name = "rt_match_diff"
        )

        # remove extra columns
        w <- which(colnames(OUT) %in% c(".id", ".rt_min", ".rt_max"))
        OUT <- OUT[, -w]

        D$data <- OUT

        M$updated <- D

        return(M)
    }
)
