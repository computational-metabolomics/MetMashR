#' @eval get_description('mz_match')
#' @export
#' @include annotation_source_class.R
#' @importFrom dplyr join_by inner_join
mz_match <- function(variable_meta,
    mz_column,
    ppm_window,
    id_column,
    ...) {
    # make rt_window length 2
    if (length(ppm_window) == 1) {
        ppm_window <- c(
            "variable_meta" = ppm_window,
            "annotations" = ppm_window
        )
    }

    # check ppm window is named if length == 2 in case user-provided
    if (!all(names(ppm_window) %in% c("variable_meta", "annotations")) |
        is.null(names(ppm_window))) {
        stop(
            "If providing two ppm windows then the vector must be named ",
            'e.g. c("variable_meta" = 5, "annotations"= 2)'
        )
    }

    out <- struct::new_struct(
        "mz_match",
        variable_meta = variable_meta,
        mz_column = mz_column,
        ppm_window = ppm_window,
        id_column = id_column,
        ...
    )
    return(out)
}



.mz_match <- setClass(
    "mz_match",
    contains = c("model"),
    slots = c(
        updated = "entity",
        variable_meta = "entity",
        mz_column = "entity",
        ppm_window = "entity",
        id_column = "entity",
        .vm_lim = "data.frame",
        .an_lim = "data.frame"
    ),
    prototype = list(
        name = "mz matching",
        description = paste0(
            "Annotations will be matched to the measured data variable meta ",
            "data.frame by determining which annotations ppm window overlaps ",
            "with the ppm ",
            "window from the measured mz."
        ),
        type = "univariate",
        predicted = "updated",
        .params = c("variable_meta", "mz_column", "ppm_window", "id_column"),
        .outputs = c("updated"),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The input annotation source with the newly generated column."
            ),
            type = "annotation_source"
        ),
        variable_meta = entity(
            name = "Variable meta data",
            description = paste0(
                "A data.frame of variable IDs and their corresponding mz ",
                "values."
            ),
            type = "data.frame"
        ),
        mz_column = entity(
            name = "mz column name",
            description = "column name of the mz values in variable_meta.",
            type = "character"
        ),
        ppm_window = entity(
            name = "ppm window",
            description = paste0(
                "ppm window to use for matching. If a single value ",
                "is provided then the same ppm is used for both variable ",
                "meta and ",
                "the annotations. A named vector can also be provided ",
                'e.g. c("variable_meta"=5,"annotations"=2) to use ",
                "different windows ',
                "for each data table."
            ),
            type = c("numeric", "integer"),
            max_length = 2,
            value = 5
        ),
        id_column = entity(
            name = "id column name",
            description = paste0(
                "column name of the variable ids in variable_meta. ",
                'id_column="rownames" will use the rownames as ids.'
            ),
            type = "character"
        )
    )
)


#' @export
#' @template model_apply
#' @usage NULL
setMethod(
    f = "model_apply",
    signature = c("mz_match", "annotation_source"),
    definition = function(M, D) {
        VM <- M$variable_meta

        # ensure numeric
        VM[[M$mz_column]] <- as.numeric(VM[[M$mz_column]])

        # use rownames for id if requested
        if (M$id_column == "rownames") {
            VM$.id <- rownames(VM)
        } else {
            VM$.id <- VM[[M$id_column]]
        }

        AN <- D$data

        # ensure numeric
        AN[[D$mz_column]] <- as.numeric(AN[[D$mz_column]])

        # calculate ppm window for variable_meta
        VM$.mz_min <- M$variable_meta[[M$mz_column]] *
            (1 - M$ppm_window[["variable_meta"]] * 1e-6)
        VM$.mz_max <- M$variable_meta[[M$mz_column]] *
            (1 + M$ppm_window[["variable_meta"]] * 1e-6)

        # calculate ppm window for annotations
        AN$.mz_min <- AN[[D$mz_column]] *
            (1 - M$ppm_window[["annotations"]] * 1e-6)
        AN$.mz_max <- AN[[D$mz_column]] *
            (1 + M$ppm_window[["annotations"]] * 1e-6)

        M@.vm_lim <- data.frame(
            VM_mz_min = VM$.mz_min,
            VM_mz_max = VM$.mz_max
        )
        M@.an_lim <- data.frame(
            AN_mz_min = AN$.mz_min,
            AN_mz_max = AN$.mz_max
        )

        OUT <- .interval_overlap_join(
            VM = VM,
            AN = AN,
            vm_value_column = M$mz_column,
            an_value_column = D$mz_column,
            vm_min_column = ".mz_min",
            vm_max_column = ".mz_max",
            an_min_column = ".mz_min",
            an_max_column = ".mz_max",
            match_id_name = "mz_match_id",
            match_value_name = "mz_match",
            match_diff_name = "mz_match_diff",
            an_ppm_diff_name = "ppm_match_diff_an",
            vm_ppm_diff_name = "ppm_match_diff_vm"
        )

        # remove extra columns
        w <- which(colnames(OUT) %in% c(".id", ".mz_min", ".mz_max"))
        OUT <- OUT[, -w]

        D$data <- OUT

        M$updated <- D

        return(M)
    }
)

#' Match two sets of numeric intervals (e.g. mz or rt windows) and return
#' every overlapping (annotation row, variable_meta row) pair.
#'
#' Uses `dplyr`'s overlap join (`join_by(overlaps(...))`, `dplyr` >= 1.1.0)
#' to find every VM row whose `[vm_min_column, vm_max_column]` window
#' overlaps an AN row's `[an_min_column, an_max_column]` window, rather than
#' the previous per-row `which()` scan plus `rep()`/`rbind()` per annotation
#' row -- at production scale (tens of thousands of variable_meta rows) that
#' per-row approach could build many-hundred-thousand-row intermediate
#' objects in a way that exhausted memory. The join itself only ever sees a
#' minimal 4-column frame from each side (never the full VM/AN tables), so
#' there's no risk of an accidental column-name collision with either
#' table's own columns.
#' @noRd
.interval_overlap_join <- function(
        VM, AN, vm_value_column, an_value_column,
        vm_min_column, vm_max_column, an_min_column, an_max_column,
        match_id_name, match_value_name, match_diff_name,
        an_ppm_diff_name = NULL, vm_ppm_diff_name = NULL) {
    vm_slim <- data.frame(
        ..vm_id = VM$.id,
        ..vm_value = VM[[vm_value_column]],
        ..vm_min = VM[[vm_min_column]],
        ..vm_max = VM[[vm_max_column]]
    )
    an_slim <- data.frame(
        ..an_row = seq_len(nrow(AN)),
        ..an_value = AN[[an_value_column]],
        ..an_min = AN[[an_min_column]],
        ..an_max = AN[[an_max_column]]
    )

    # overlaps(x_lower, x_upper, y_lower, y_upper) matches wherever
    # [..an_min, ..an_max] overlaps [..vm_min, ..vm_max] in any capacity
    # (bounds="[]" by default, i.e. <=/>=, matching the previous behaviour)
    joined <- dplyr::inner_join(
        an_slim, vm_slim,
        by = dplyr::join_by(overlaps(..an_min, ..an_max, ..vm_min, ..vm_max))
    )

    matched <- AN[joined$..an_row, , drop = FALSE]
    diff <- joined$..vm_value - joined$..an_value
    matched[[match_id_name]] <- joined$..vm_id
    matched[[match_diff_name]] <- diff
    if (!is.null(an_ppm_diff_name)) {
        matched[[an_ppm_diff_name]] <- 1e6 * (diff / joined$..an_value)
    }
    if (!is.null(vm_ppm_diff_name)) {
        matched[[vm_ppm_diff_name]] <- 1e6 * (-diff / joined$..vm_value)
    }
    matched[[match_value_name]] <- joined$..vm_value

    # AN rows with no overlapping VM row keep a single NA-filled record
    unmatched_idx <- setdiff(seq_len(nrow(AN)), joined$..an_row)
    if (length(unmatched_idx) > 0) {
        un <- AN[unmatched_idx, , drop = FALSE]
        un[[match_id_name]] <- NA
        un[[match_diff_name]] <- NA
        if (!is.null(an_ppm_diff_name)) un[[an_ppm_diff_name]] <- NA
        if (!is.null(vm_ppm_diff_name)) un[[vm_ppm_diff_name]] <- NA
        un[[match_value_name]] <- NA
        matched <- rbind(matched, un)
    }

    return(matched)
}
