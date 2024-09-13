#' @eval get_description('mwb_compound_lookup')
#' @export
#' @include annotation_source_class.R rest_api_class.R zzz.R
#' @family {REST API's}
mwb_compound_lookup <- function(
        input_item = "inchi_key",
        query_column,
        output_item = "pubchem_id",
        suffix = "_mwb",
        ...) {
    allowed <- c(
        "all",
        "regno",
        "formula",
        "exactmass",
        "inchi_key",
        "name",
        "sys_name",
        "smiles",
        "lm_id",
        "pubchem_cid",
        "hmdb_id",
        "chebi_id",
        "metacyc_id",
        "classification",
        "png"
    )
    if (!all(output_item %in% allowed)) {
        stop("Please specify a valid output item.")
    }
    
    if (length(output_item) > 1) {
        output_item <- paste0(output_item, collapse = ",")
    }
    
    out <- struct::new_struct(
        "mwb_compound_lookup",
        input_item = input_item,
        query_column = query_column,
        output_item = output_item,
        suffix = suffix,
        ...
    )
    return(out)
}

.mwb_compound_lookup <- setClass(
    "mwb_compound_lookup",
    contains = "rest_api",
    slots = c(
        input_item = "enum",
        output_item = "entity"
    ),
    prototype = list(
        name = "Convert to/from kegg identifiers",
        description = paste0(
            "Searches MetabolomicsWorkbench for compound ",
            "identifiers."
        ),
        type = "rest_api",
        predicted = "updated",
        libraries = c("metabolomicsWorkbenchR", "dplyr"),
        .params = c("input_item", "output_item"),
        .outputs = c("updated"),
        base_url = entity(
            name = "Base URL",
            description = "The base URL of the API.",
            type = c("character"),
            value = "https://www.metabolomicsworkbench.org/rest",
            max_length = 1
        ),
        url_template = entity(
            name = "URL template",
            description = paste0(
                "A template describing how the URL should be ",
                "constructed from the base URL and input parameters. e.g. ",
                "<base_url>/<context>/<input_item>/<search_term>/json.",
                "The url will be constructed by replacing the values ",
                "enclosed in <> with the value from corresponding input ",
                "parameter of the rest_api object."
            ),
            max_length = 1,
            value = paste0(
                "<base_url>/compound/<input_item>/<query_column>/<output_item>"
            )
        ),
        status_codes = entity(
            name = "Status codes",
            description = paste0(
                "Named list of status codes and function indicating how to ",
                "respond. Should minimally contain a function to parse a ",
                "successful response for status code 200. Any codes not ",
                "provided will be passed to httr::stop_for_status()."
            ),
            type = "list",
            value = list(
                "200" = .parse_json_mwb,
                "404" = function(...) {
                    return(NULL)
                }
            ),
            max_length = Inf
        ),
        input_item = enum(
            name = "Input item",
            description = paste0(
                "A valid input item for the compound context (see ",
                "https://www.metabolomicsworkbench.org/tools/mw_rest.php). ",
                "The values in the query_column should be of this type."
            ),
            type = "character",
            max_length = 1,
            value = "inchi_key",
            allowed = c(
                "regno",
                "formula",
                "inchi_key",
                "lm_id",
                "pubchem_id",
                "hmdb_id",
                "kegg_id",
                "chebi_id",
                "metacyc_id",
                "abbrev"
            )
        ),
        output_item = entity(
            name = "Output item",
            description = paste0(
                "A comma separated list of Valid output items for the ",
                "compound context (see ",
                "https://www.metabolomicsworkbench.org/tools/mw_rest.php)."
            ),
            type = "character",
            max_length = 1,
            value = "inchi_key"
        )
    )
)
