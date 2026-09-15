# the LIPID MAPS bulk "all" download starts with a date-stamp line before
# the real tab-separated header row, so the first line must be skipped
.lipidmaps_import_fun <- function(path) {
    df <- utils::read.delim(
        path,
        skip = 1,
        header = TRUE,
        quote = "",
        stringsAsFactors = FALSE,
        na.strings = c("", "NA")
    )
    return(df)
}

#' @eval get_description('lipidmaps_database')
#' @export
#' @include annotation_database_class.R BiocFileCache_database_class.R
#' @family database
lipidmaps_database <- function(bfc_path = NULL,
    resource_name = "MetMashR_lipidmaps",
    ...) {
    out <- struct::new_struct(
        "lipidmaps_database",
        source = paste0(
            "https://www.lipidmaps.org/rest/compound/lm_id/LM/all/download"
        ),
        bfc_path = bfc_path,
        resource_name = resource_name,
        ...
    )
    return(out)
}

.lipidmaps_database <- setClass(
    "lipidmaps_database",
    contains = "BiocFileCache_database",
    prototype = list(
        name = "LIPID MAPS database",
        description = paste0(
            "Imports the full LIPID MAPS Structure Database (LMSD) bulk ",
            "compound export (~50,000 lipids), cached locally with ",
            "BiocFileCache so it is only downloaded once rather than queried ",
            "per-row. Columns include lm_id, name, abbrev, core, main_class, ",
            "sub_class, formula, inchi, inchi_key, kegg_id, hmdb_id, ",
            "chebi_id, pubchem_cid and smiles -- a single database_lookup() ",
            "against this table can replace many individual ",
            "lipidmaps_lookup() REST queries."
        ),
        type = "lipidmaps_source",
        libraries = c("BiocFileCache"),
        import_fun = .set_entity_value(
            obj = "BiocFileCache_database",
            param_id = "import_fun",
            value = .lipidmaps_import_fun
        ),
        bfc_fun = .set_entity_value(
            obj = "BiocFileCache_database",
            param_id = "bfc_fun",
            value = cache_as_is
        )
    )
)
