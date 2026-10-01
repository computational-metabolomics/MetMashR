#' @eval get_description('mspurity_source')
#' @include annotation_source_class.R lcms_table_class.R
#' @family annotation sources
#' @family annotation tables
#' @export mspurity_source
mspurity_source <- function(source,
    tag = "msPurity",
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
        colnames(data) <- c(id_column, mz_column, rt_column)
    }

    # new object
    out <- new_struct(
        "mspurity_source",
        source = source,
        tag = tag,
        mz_column = mz_column,
        rt_column = rt_column,
        id_column = id_column,
        data = data,
        .required = c(mz_column, id_column, rt_column),
        ...
    )
    return(out)
}


.mspurity_source <- setClass(
    "mspurity_source",
    contains = c("lcms_table"),
    prototype = list(
        name = "msPurity source",
        description = paste0(
            "An annotation source for importing an annotation table from the
            format created by the `msPurity` package."
        ),
        type = "annotation source",
        libraries = "msPurity",
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
        )
    )
)


# NOTE: this used to be a `model_apply(M, D)` method with a two-argument
# signature c("mspurity_source", "lcms_table"), writing its result into an
# `M$imported` output slot. `mspurity_source` never declared that slot (it
# contained only `annotation_source`, which has no `predicted`/output
# machinery -- that's a `model`-only concept), so every call errored with
# `"imported" is not a valid param, output or column name`. Following
# `cd_source`/`ls_source`'s pattern instead: `mspurity_source` now contains
# `lcms_table` directly (so it has its own mz_column/rt_column/id_column)
# and is populated in place via a single-argument `read_source(obj)` method,
# exactly like those two.
#' @export
#' @rdname read_source
setMethod(
    f = "read_source",
    signature = c("mspurity_source"),
    definition = function(obj) {
        M <- obj

        # check for zero content
        check <- readLines(M$source)
        if (check[1] == "" | check[1] == "\"\"") {
            # its empty, so create data.frame with no rows
            cols <- c(
                "pid",
                "grpid",
                "mz",
                "mzmin",
                "mzmax",
                "rt",
                "rtmin",
                "rtmax",
                "npeaks",
                "sample",
                "peakidx",
                "ms_level",
                "grp_name",
                "lpid",
                "mid",
                "dpc",
                "rdpc",
                "cdpc",
                "mcount",
                "allcount",
                "mpercent",
                "library_rt",
                "query_rt",
                "library_rt_diff",
                "library_precursor_mz",
                "query_precursor_mz",
                "library_ppm_diff",
                "library_precursor_ion_purity",
                "query_precursor_ion_purity",
                "library_accession",
                "library_precursor_type",
                "library_entry_name",
                "inchikey",
                "library_table_name",
                "library_compound_name",
                "id"
            )
            df <- data.frame(matrix(NA, nrow = 0, ncol = length(cols)))
            colnames(df) <- cols

            obj$data <- df
            return(obj)
        }

        mtox_output <- read.csv(file = M$source, sep = ",", row.names = 1)

        # split library ascension
        S <- lapply(mtox_output$library_accession, function(x) {
            s <- strsplit(x = x, split = "|", fixed = TRUE)[[1]]
            s <- trimws(s)
            # remove MZ and RT from values
            s <- gsub("MZ:", "", s, fixed = TRUE)
            s <- gsub("RT:", "", s, fixed = TRUE)
            names(s) <- paste0(
                "library_accession.",
                c("MZ", "RT", "name", "ion", "hmdb_id", "assay"),
                sep = ""
            )
            df <- as.data.frame(t(as.data.frame(s)))
            rownames(df) <- names(x)
            return(df)
        })
        S <- do.call(rbind, S)

        # append to annotations
        mtox_output <- cbind(mtox_output, S)

        # convert to char and add id
        mtox_output$id <- as.character(seq_len(nrow(mtox_output)))

        # calc ppm diff
        mtox_output$library_ppm_diff <-
            1e6 * (mtox_output$query_precursor_mz -
                mtox_output$library_precursor_mz) /
                mtox_output$library_precursor_mz

        # make ions consistent with CD
        ions <- mtox_output$library_accession.ion
        # any ion ending with + or - becomes +1 or -1
        ions <- gsub("[\\+]+$", "+1", ions, perl = TRUE)
        ions <- gsub("[\\-]+$", "-1", ions, perl = TRUE)
        mtox_output$library_accession.ion <- ions

        obj$data <- mtox_output

        return(obj)
    }
)
