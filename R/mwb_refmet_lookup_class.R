#' @eval get_description('mwb_refmet_lookup')
#' @export
#' @include annotation_source_class.R rest_api_class.R rest_api_parsers.R
#' @family REST API's
mwb_refmet_lookup <- function(query_column,
    suffix = "_refmet",
    ...) {
    out <- struct::new_struct(
        "mwb_refmet_lookup",
        query_column = query_column,
        suffix = suffix,
        ...
    )
    return(out)
}


.mwb_refmet_lookup <- setClass(
    "mwb_refmet_lookup",
    contains = c("rest_api"),
    prototype = list(
        name = "Metabolomics Workbench RefMet lookup",
        description = paste0(
            "Matches a reported compound name (which may be a synonym, ",
            "conjugate acid/base form, or otherwise non-canonical name) ",
            "to its RefMet standardised name and identifiers, using the ",
            "Metabolomics Workbench RefMet REST API. Intended as a ",
            "substitute for the (now defunct) Chemical Translation ",
            "Service (CTS): the original 'PubChem Identifier Exchange + ",
            "CTS' translation route described by some published ",
            "metabolite-merging methods can no longer be reproduced, ",
            "since CTS has been shut down and its replacement, CTS-Lite, ",
            "does not support name-based lookups. RefMet's `match` ",
            "endpoint plays a similar synonym-resolution role, and its ",
            "curated picks (e.g. preferring the biologically-relevant ",
            "stereoisomer/protonation state over a generic entry) often ",
            "differ usefully from a plain PubChem name search."
        ),
        type = "rest_api",
        predicted = "updated",
        libraries = "httr",
        base_url = entity(
            name = "Base URL",
            description = "The base URL of the API.",
            type = "character",
            value = "https://www.metabolomicsworkbench.org/rest/refmet",
            max_length = 1
        ),
        url_template = entity(
            name = "URL template",
            description = paste0(
                "A template describing how the URL should be ",
                "constructed from the base URL and input parameters. ",
                "The url will be constructed by replacing the values ",
                "enclosed in <> with the value from corresponding input ",
                "parameter of the rest_api object."
            ),
            value = "<base_url>/match/<query_column>/name",
            max_length = 1
        ),
        status_codes = entity(
            name = "Status codes",
            description = paste0(
                "Named list of status codes and function indicating how ",
                "to respond. Should minimally contain a function to parse ",
                "a response for status code 200."
            ),
            type = "list",
            value = list(
                "200" = .parse_mwb_refmet_match,
                "404" = function(...) {
                    return(NULL)
                }
            ),
            max_length = Inf
        ),
        suffix = .set_entity_value(
            obj = "rest_api",
            param_id = "suffix",
            value = "_refmet"
        )
    )
)
