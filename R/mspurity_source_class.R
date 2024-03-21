#' @eval get_description('mspurity_source')
#' @include annotation_source_class.R
#' @family {annotation sources}
#' @export mspurity_source
mspurity_source <- function(source,
                            tag = "msPurity",
                            ...) {
    # new object
    out <- new_struct(
        "mspurity_source",
        source = source,
        tag = tag,
        ...
    )
    return(out)
}


.mspurity_source <- setClass(
    "mspurity_source",
    contains = c("annotation_source"),
    prototype = list(
        name = "msPurity source",
        description = paste0(
            "An annotation source for importing an annotation table from the
            format created by the `msPurity` package."
        ),
        type = "annotation source",
        libraries = "msPurity"
    )
)


#' @export
setMethod(
    f = "model_apply",
    signature = c("mspurity_source", "lcms_table"),
    definition = function(M, D) {
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

            D$data <- df
            D$mz_column <- "mz"
            D$rt_column <- "rt"
            D$id_column <- "id"
            D$tag <- M$tag
            M$imported <- D

            return(M)
        }

        mtox_output <- read.csv(file = M$source, sep = ",", row.names = 1)

        # split library ascension
        S <- lapply(mtox_output$library_accession, function(x) {
            s <- strsplit(x = x, split = "|", fixed = TRUE)[[1]]
            s <- trimws(s)
            # remove MZ and RT from values
            s <- gsub("MZ:", "", s, fixed = TRUE)
            s <- gsub("RT:", "", s, fixed = TRUE)
            names(s) <- paste0("library_accession.",
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

        # add extra columns if requested
        if (length(M$add_cols) > 0) {
            for (g in seq_len(length(M$add_cols))) {
                mtox_output[[names(M$add_cols)[g]]] <- M$add_cols[[g]]
            }
        }

        D$data <- mtox_output

        D$mz_column <- "mz"
        D$rt_column <- "rt"
        D$id_column <- "id"
        D$tag <- M$tag

        M$imported <- D

        return(M)
    }
)
