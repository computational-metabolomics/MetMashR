#' @eval get_description('mwb_study_source')
#' @include annotation_source_class.R
#' @family annotation sources
#' @export
mwb_study_source <- function(source,
    tag = "MWB",
    analysis_id = NULL,
    data = NULL,
    ...) {
    if (is.null(data)) {
        data <- data.frame()
    }

    if (nrow(data) == 0 & ncol(data) == 0) {
        data <- data.frame(
            study_id = character(0),
            analysis_id = character(0),
            analysis_summary = character(0),
            metabolite_name = character(0),
            refmet_name = character(0),
            pubchem_id = character(0),
            other_id = character(0),
            other_id_type = character(0)
        )
    }

    # new object
    out <- new_struct(
        "mwb_study_source",
        source = source,
        tag = tag,
        analysis_id = analysis_id,
        data = data,
        ...
    )
    return(out)
}


.mwb_study_source <- setClass(
    "mwb_study_source",
    contains = c("annotation_source"),
    slots = c(
        analysis_id = "entity"
    ),
    prototype = list(
        name = "Metabolomics Workbench study source",
        description = paste0(
            "Imports the reported metabolite list for a Metabolomics ",
            "Workbench study, using the `metabolomicsWorkbenchR` package. ",
            "Only the identifiers a study depositor chose to report are ",
            "returned as-is (e.g. `metabolite_name`, and where present ",
            "`refmet_name`/`pubchem_id`/`other_id`) - no translation or ",
            "enrichment is performed here. To reproduce the scenario of ",
            "annotation software output that has names but no ",
            "standardised identifiers (the case MetMashR's mashing steps ",
            "are designed for), downstream workflow steps should use only ",
            "the `metabolite_name` column and treat any pre-existing ",
            "`refmet_name`/`pubchem_id`/`other_id` values as optional ",
            "validation data, not as workflow input."
        ),
        type = "annotation source",
        libraries = "metabolomicsWorkbenchR",
        .params = c("analysis_id"),
        source = entity(
            name = "Metabolomics Workbench study id",
            description = paste0(
                "A Metabolomics Workbench study identifier e.g. ",
                '"ST001039".'
            ),
            type = "character",
            max_length = 1
        ),
        analysis_id = entity(
            name = "Analysis id",
            description = paste0(
                "Optionally restrict the imported metabolites to one or ",
                "more specific analysis ids within the study (e.g. a ",
                "single LC-MS assay). If `NULL` (the default), metabolites ",
                "for all analyses in the study are returned."
            ),
            type = c("character", "NULL"),
            value = NULL,
            max_length = Inf
        ),
        data = .set_entity_value(
            obj = "annotation_source",
            param_id = "data",
            value = data.frame(
                study_id = character(0),
                analysis_id = character(0),
                analysis_summary = character(0),
                metabolite_name = character(0),
                refmet_name = character(0),
                pubchem_id = character(0),
                other_id = character(0),
                other_id_type = character(0)
            )
        )
    )
)


#' @export
#' @rdname read_source
setMethod(
    f = "read_source",
    signature = c("mwb_study_source"),
    definition = function(obj) {
        df <- metabolomicsWorkbenchR::do_query(
            context = "study",
            input_item = "study_id",
            input_value = obj$source,
            output_item = "metabolites"
        )

        if (!is.null(obj$analysis_id)) {
            df <- df[df$analysis_id %in% obj$analysis_id, , drop = FALSE]
        }

        obj$data <- as.data.frame(df)

        return(obj)
    }
)
