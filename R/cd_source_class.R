#' @eval get_description('cd_source')
#' @include annotation_source_class.R lcms_table_class.R
#' @family annotation sources
#' @family annotation tables
#' @rawNamespace import(dplyr, except = as_data_frame)
#' @export
cd_source <- function(
        source,
        sheets = c(1, 1),
        tag = "CD",
        mz_column = "mz",
        rt_column = "rt",
        id_column = "id",
        data = NULL,
        ...) {
    if (is.null(data)) {
        data <- data.frame()
    }
    
    if (nrow(data) == 0 & ncol(data) == 0) {
        data <- data.frame(
            id = character(0),
            mz = numeric(0),
            rt = numeric(0)
        )
        colnames(data) <- c(
            id_column,
            mz_column,
            rt_column
        )
    }
    
    # new object
    out <- new_struct(
        "cd_source",
        source = source,
        tag = tag,
        mz_column = mz_column,
        rt_column = rt_column,
        id_column = id_column,
        .required = c(mz_column, id_column, rt_column),
        data = data,
        sheets= sheets,
        ...
    )
    return(out)
}

.cd_source <- setClass(
    "cd_source",
    contains = c("lcms_table"),
    slots = c(
        sheets = "entity"
    ),
    prototype = list(
        source = entity(
            name = "CD source file(s)",
            description = paste0(
                "The path to the Compound Discoverer Excel files to import. ",
                "Both the compounds and isomers file should be included, in ",
                "that order."
            ),
            type = "character",
            max_length = 2,
        ),
        sheets = entity(
            name = "Sheet names",
            description = paste0(
                "The name or index of the sheets to read from the source ",
                "file(s). A sheet should be provided for each input file."
            ),
            value = c(1, 1),
            type = c("character", "numeric", "integer"),
            max_length = 2
        ),
        data = .set_entity_value(
            obj = "lcms_table",
            param_id = "data",
            value = data.frame(
                id = character(0),
                mz = character(0),
                rt = character(0)
            )
        ),
        id_column = .set_entity_value(
            obj = "lcms_table",
            param_id = "id_column",
            value = "id"
        ),
        mz_column = .set_entity_value(
            obj = "lcms_table",
            param_id = "mz_column",
            value = "mz"
        ),
        rt_column = .set_entity_value(
            obj = "lcms_table",
            param_id = "rt_column",
            value = "rt"
        ),
        .params = c("sheets")
    )
)



#' @export
#' @rdname read_source
setMethod(
    f = "read_source",
    signature = c("cd_source"),
    definition = function(obj) {
        M <- obj
        # read files
        TB1 <- .read_cd_compounds_file(M, 1)
        TB2 <- .read_cd_isomers_file(M, 2)
        
        # join blue and orange
        L <- list()
        L[[1]] <- left_join(TB1$blue, TB1$orange,
                            by = join_by(blue_id),
                            suffix = c(".blue", ".orange")
        )
        L[[2]] <- left_join(TB2$blue, TB2$orange,
                            by = join_by(blue_id),
                            suffix = c(".blue", ".orange")
        )
        
        # join grey
        L[[1]] <- left_join(L[[1]], TB1$grey, by = join_by(
            blue_id == blue_id,
            orange_id == orange_id
        ), suffix = c(".blueorange", ".grey"))
        
        # summarise
        L[[1]] <- L[[1]] %>%
            group_by(blue_id, Ion) %>%
            summarise(
                Compound = unique(Name),
                Formula = unique(Formula),
                RT = mean(as.numeric(.data[["RT [min].grey"]]) * 60,
                          na.rm = TRUE
                ),
                mzcloud_score = mean(as.numeric(mzCloud.Best.Match),
                                     na.rm = TRUE
                ),
                Charge = unique(Charge),
                mz = mean(as.numeric(.data[["m/z.grey"]]), na.rm = TRUE),
                area = max(Area),
                file_count = n()
            )
        
        # join compounds and isomers
        OUT <- left_join(L[[1]], L[[2]],
                         by = join_by(blue_id),
                         relationship = "many-to-many", suffix = c(".cpd", ".iso")
        )
        
        
        OUT <- OUT %>%
            # select relevant columns
            select(
                all_of(c(
                    compound = "Name.orange",
                    ion = "Ion",
                    formula = "Formula.orange",
                    mz = "mz",
                    rt = "RT",
                    file_count = "file_count",
                    area = "area",
                    kegg_id = "KEGG ID",
                    mzcloud_score = "Match",
                    mzcloud_confidence = "Confidence",
                    mzcloud_id = "mzCloud ID",
                    compound_match = "Compound Match",
                    library_ppm_diff = "DeltaMass [ppm]",
                    theoretical_mass = "Molecular Weight",
                    cd_id = "blue_id",
                    class = "Compound Class",
                    reference_ion = "Reference.Ion"
                ))
            ) %>%
            # convert some to numeric
            mutate(
                across(c(
                    "mz",
                    "rt",
                    "file_count",
                    "area",
                    "mzcloud_score",
                    "mzcloud_confidence",
                    "library_ppm_diff",
                    "theoretical_mass"
                ), as.numeric)
            )
        
        # calc theoretical mz
        OUT$theoretical_mz <- OUT$mz * 1e6 /
            (OUT$library_ppm_diff + 1e6)
        
        # id row id
        OUT$id <- as.character(seq_len(nrow(OUT)))
        
        # update object
        M$data <- as.data.frame(OUT)
        
        # return
        return(M)
    }
)


# Compound Discoverer's Excel exports embed repeated header rows inside the
# data itself: whenever `cd[[marker_col]]` reads the literal string "Tags",
# that row holds display labels for the block of records that follows (up to
# the next such marker). The label row is identical throughout the file, so
# only the first occurrence is needed. Returns a named character vector
# suitable for `select(all_of(.))` (name = label from the marker row, value
# = the underlying raw column name), or NULL if `marker_col` never reads
# "Tags" (e.g. some exports have no "grey"/per-file block at all).
.cd_marker_row_rename_map <- function(cd, marker_col) {
    w <- which(cd[[marker_col]] == "Tags")
    if (length(w) == 0) {
        return(NULL)
    }
    header_row <- cd[w[1], ]
    map <- unlist(header_row[which(header_row != "")])
    # blue_id/orange_id are columns we add ourselves below (not part of the
    # original export), so their marker-row "label" is just noise -- keep
    # their own name as the label instead
    added <- names(map) %in% c("blue_id", "orange_id")
    map[added] <- names(map)[added]
    setNames(names(map), map)
}

# Assigns a running block id to `marker_col`: a new id starts every time it
# reads "TRUE" or "FALSE" (a record boundary), 0 for any rows before the
# first boundary.
.cd_block_ids <- function(marker_col) {
    cumsum(marker_col %in% c("TRUE", "FALSE"))
}

.read_cd_isomers_file <- function(M, idx) {
    cd <- openxlsx::read.xlsx(
        M$source[idx],
        M$sheets[idx]
    )

    # ids for joining blue (compound-level) and orange (isomer-level) rows
    cd$blue_id <- .cd_block_ids(cd$Checked)
    cd$orange_id <- .cd_block_ids(cd$Name)

    ## blue rows
    blue <- cd %>% filter(Checked == "FALSE")

    ## orange rows
    orange <- cd %>%
        select(all_of(.cd_marker_row_rename_map(cd, "Checked"))) %>%
        filter(Checked %in% c("TRUE", "FALSE"))

    return(
        list(
            blue = blue,
            orange = orange
        )
    )
}


.read_cd_compounds_file <- function(M, idx) {
    cd <- openxlsx::read.xlsx(
        M$source[idx],
        M$sheets[idx]
    )

    # ids for joining blue (compound-level), orange (isomer-level) and grey
    # (per-file/per-adduct feature-level) rows
    cd$blue_id <- .cd_block_ids(cd$Checked)
    cd$orange_id <- .cd_block_ids(cd$Name)

    ## blue rows
    blue <- cd %>%
        filter(Checked == "FALSE") %>%
        select(-orange_id)

    ## orange rows
    orange <- cd %>%
        select(all_of(.cd_marker_row_rename_map(cd, "Checked"))) %>%
        filter(Checked %in% c("TRUE", "FALSE"))

    ## grey rows -- not every export includes this block; fall back to an
    ## empty (join-key-only) table rather than erroring when it's absent
    grey_map <- .cd_marker_row_rename_map(cd, "Name")
    grey <- if (is.null(grey_map)) {
        data.frame(blue_id = numeric(0), orange_id = numeric(0))
    } else {
        cd %>%
            select(all_of(grey_map)) %>%
            filter(Checked == "FALSE")
    }

    return(
        list(
            blue = blue,
            orange = orange,
            grey = grey
        )
    )
}
