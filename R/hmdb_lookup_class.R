#' @eval get_description('hmdb_lookup')
#' @export
#' @include annotation_source_class.R rest_api_class.R
hmdb_lookup <- function(query_column,
                        suffix = "_hmdb",
                        output = "inchikey",
                        ...) {
    out <- struct::new_struct(
        "hmdb_lookup",
        query_column = query_column,
        suffix = suffix,
        output = output,
        ...
    )

    return(out)
}

.hmdb_lookup <- setClass(
    "hmdb_lookup",
    contains = c("rest_api"),
    slots = c(
        search_by = "enum",
        output = "enum",
        records = "enum"
    ),
    prototype = list(
        name = "Compound ID lookup via pubchem",
        description = paste0(
            "Requests HMBD records based on HMDB identifiers. "
        ),
        type = "rest_api",
        predicted = "updated",
        .params = c("output"),
        libraries = c("XML"),
        base_url = .set_entity_value(
            obj = "rest_api",
            param_id = "base_url",
            value = "http://www.hmdb.ca/metabolites"
        ),
        url_template = .set_entity_value(
            obj = "rest_api",
            param_id = "base_url",
            value = "<base_url>/<query_column>.xml"
        ),
        output = enum(
            name = "Output",
            description =
                "The value returned from the HMDB xml.",
            allowed = c("inchikey", "smiles"),
            value = "inchikey"
        ),
        status_codes = .set_entity_value(
            obj = "rest_api",
            param_id = "status_codes",
            value = list(
                "200" = .parse_hmdb_xml_ids,
                "404" = function(...) {
                    return(NULL)
                }
            )
        )
    )
)
