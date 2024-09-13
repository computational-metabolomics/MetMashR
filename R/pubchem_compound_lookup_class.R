#' @eval get_description('pubchem_compound_lookup')
#' @export
#' @include annotation_source_class.R rest_api_class.R
pubchem_compound_lookup <- function(
        query_column,
        search_by,
        suffix = "_pubchem",
        output = "cids",
        records = "best",
        ...) {
    out <- struct::new_struct(
        "pubchem_compound_lookup",
        query_column = query_column,
        search_by = search_by,
        suffix = suffix,
        output = output,
        records = records,
        ...
    )
    
    return(out)
}

.pubchem_compound_lookup <- setClass(
    "pubchem_compound_lookup",
    contains = c("rest_api"),
    slots = c(
        search_by = "enum",
        output = "enum",
        records = "enum"
    ),
    prototype = list(
        name = "Compound ID lookup via PubChem",
        description = paste0("Uses the PubChem API to search for CID based on
                             the input annotation column."),
        type = "rest_api",
        predicted = "updated",
        .params = c("search_by", "output", "records"),
        base_url = entity(
            name = "Base URL",
            description = "The base URL of the API.",
            type = c("character"),
            value = "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound",
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
                "parameter of the rest_api object. A term preceeded by . will ",
                "assume substitute values from a column name from the  ",
                "annotation table."
            ),
            value = c(
                "<base_url>/<search_by>/<query_column>/<output>/JSON"
            ),
            max_length = 1
        ),
        query_column = entity(
            name = "Annotation column name",
            description = paste0(
                "The column name to use as the reference for searching the ",
                'database e.g. "HMBD_ID"'
            ),
            type = c("character"),
            max_length = 1
        ),
        search_by = enum(
            name = "Search by term",
            description = paste0(
                "The PubChem domain to search for matches to the ",
                "annotation_column"
            ),
            allowed = c("cid", "name", "smiles", "inchikey"),
            value = "cid"
        ),
        output = enum(
            name = "Output",
            description =
                "The value returned from the pubchem database.",
            allowed = c("cids", "synonyms", "sids", "description"),
            value = "cids"
        ),
        records = enum(
            name = "Returned record(s)",
            description = c(
                paste0(
                    "Sometimes there are multiple matches to the PubChem, ",
                    "database especially when searhcing by name."
                ),
                "best" = "Return only the best matching record.",
                "all" = "Return all matching records."
            ),
            allowed = c("best", "all"),
            value = "best"
        ),
        status_codes = entity(
            name = "Status codes",
            description = paste0(
                "Named list of status codes and function indicating how to ",
                "respond. Should minimally contain a function to parse a ",
                "response for status code 200."
            ),
            type = "list",
            value = list(
                "200" = .parse_pubchem_json,
                "404" = function(...) {
                    return(NULL)
                },
                "400" = function(...) {
                    return(NULL)
                }
            ),
            max_length = Inf
        )
    )
)
