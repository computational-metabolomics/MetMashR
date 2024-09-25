#' @eval get_description('opsin_lookup')
#' @export
#' @include annotation_source_class.R rest_api_class.R
opsin_lookup <- function(
        query_column,
        suffix = "_opsin",
        output = "cids",
        ...) {
    out <- struct::new_struct(
        "opsin_lookup",
        query_column = query_column,
        suffix = suffix,
        output = output,
        ...
    )
    
    return(out)
}

.opsin_lookup <- setClass(
    "opsin_lookup",
    contains = c("rest_api"),
    slots = c(
        search_by = "enum",
        output = "enum",
        records = "enum"
    ),
    prototype = list(
        name = "Compound ID lookup via OPSIN",
        description = paste0(
            "Uses the [OPSIN API](https://opsin.ch.cam.ac.uk/) to search for ",
            "identifers based on the input annotation column."
        ),
        type = "rest_api",
        predicted = "updated",
        .params = c("output"),
        citations = list(
            bibentry(
                bibtype = "article",
                author = as.person(paste0(
                    "Lowe, Daniel M. and Corbett, Peter T. and ",
                    "Murray-Rust, Peter and Glen, Robert C."
                )),
                title = paste0('Chemical Name to Structure: OPSIN, an Open ",
                    "Source Solution'),
                journal = "Journal of Chemical Information and Modeling",
                volume = 51,
                number = 3,
                pages = "793-753",
                year = 2011,
                doi = "10.1021/ci100384d"
            )
        ),
        base_url = entity(
            name = "Base URL",
            description = "The base URL of the API.",
            type = c("character"),
            value = "https://opsin.ch.cam.ac.uk/opsin",
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
                "<base_url>/<query_column>.<output>"
            ),
            max_length = 1
        ),
        query_column = entity(
            name = "Annotation column name",
            description = paste0(
                "The column name to use as the reference for searching the ",
                'database e.g. "compound_name". OPSIN expect molecule names ',
                "as input."
            ),
            type = c("character"),
            max_length = 1
        ),
        output = enum(
            name = "Output",
            description =
                "The value returned from the pubchem database.",
            allowed = c("inchi", "stdinchi", "stdinchikey", "smi"),
            value = "stdinchikey"
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
                "200" = .parse_opsin,
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
