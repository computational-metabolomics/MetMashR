#' @eval get_description('classyfire_lookup')
#' @export
#' @include annotation_source_class.R rest_api_class.R zzz.R
#' @family REST API's
classyfire_lookup <- function(
    query_column,
    output_items = "kingdom",
    output_fields = "name",
    suffix = "_cf",
    ...) {
    out <- struct::new_struct("classyfire_lookup",
        query_column = query_column,
        output_items = output_items,
        output_fields = output_fields,
        suffix = suffix,
        ...
    )
    return(out)
}

.classyfire_lookup <- setClass(
    "classyfire_lookup",
    contains = c("rest_api"),
    slots = c(
        output_items = "enum",
        output_fields = "enum"
    ),
    prototype = list(
        name = "Query ClassyFire database",
        description = paste0(
            "Queries the ClassyFire database by inchikey to ",
            "obtain chemical ontology information."
        ),
        type = "classyfire_api",
        predicted = "updated",
        libraries = c("dplyr", "httr"),
        .params = c("output_items", "output_fields"),
        .outputs = c("updated"),
        base_url = entity(
            name = "Base URL",
            description = "The base URL of the API.",
            type = c("character"),
            value = "http://classyfire.wishartlab.com/entities",
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
            max_length = Inf,
            value = "<base_url>/<query_column>.json"
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
                "200" = .parse_json_classyfire,
                "404" = function(...) {
                    return(NULL)
                }
            ),
            max_length = Inf
        ),
        output_items = enum(
            name = "Output item",
            description = paste0(
                "The names of the items to return from the results of the ",
                'search. Can include any number of "kingdom", "superclass", ',
                '"class", "subclass", "direct_parent", "intermediate_nodes", ',
                '"substituents", "smiles", "molecular_framework", ',
                '"description", "ancestors", "predicted_chebi_terms". ',
                'Keyword ".all" may be used to return all ',
                "items."
            ),
            type = c("character"),
            value = "kingdom",
            max_length = Inf,
            allowed = c(
                "kingdom", "superclass", "class", "subclass", "direct_parent",
                "intermediate_nodes", "substituents", "smiles",
                "molecular_framework", "description", "ancestors",
                "predicted_chebi_terms", ".all"
            )
        ),
        output_fields = enum(
            name = "Output field",
            description = paste0(
                "The names of fields to return for each output_item. Can ",
                'include any of "name", "description", "chemont_id" and ',
                '"url". Keyword ".all" may be used to return all fields. Some ',
                "items do not have fields, so output_category is ignored."
            ),
            type = c("character"),
            value = "name",
            max_length = Inf,
            allowed = c("name", "description", "chemont_id", "url", ".all")
        )
    )
)
