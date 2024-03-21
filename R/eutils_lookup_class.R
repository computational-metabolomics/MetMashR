#' @eval get_description('eutils_lookup')
#' @export
#' @include annotation_source_class.R rest_api_class.R
eutils_lookup <- function(query_column,
                          database,
                          term,
                          result_fields = "idlist",
                          ...) {
    out <- struct::new_struct(
        "eutils_lookup",
        query_column = query_column,
        database = database,
        term = term,
        result_fields = result_fields,
        ...
    )

    return(out)
}

.parse_eutils_json <- function(response, params) {
    response <- httr::content(response, as = "text", encoding = "UTF-8")
    J <- jsonlite::fromJSON(response)

    out <- J$esearchresult[params$result_fields]

    out <- lapply(out, as.data.frame)
    out <- do.call(cbind, out)

    if (nrow(out) > 0) {
        colnames(out) <- params$result_fields
    } else {
        out <- NULL
    }

    return(out)
}

.eutils_lookup <- setClass(
    "eutils_lookup",
    contains = c("rest_api"),
    slots = c(
        database = "entity",
        term = "entity",
        result_fields = "entity"
    ),
    prototype = list(
        name = "NCBI E-utils query",
        description = paste0(
            "Submit a query to one of the NCBI E-utils databases. See ",
            "https://www.ncbi.nlm.nih.gov/books/NBK25501/ for details."
        ),
        type = "rest_api",
        predicted = "updated",
        .params = c("database", "term", "result_fields"),
        .encode_reserved = FALSE,
        base_url = entity(
            name = "Base URL",
            description = "The base URL of the API.",
            type = c("character"),
            value = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils",
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
            value = paste0(
                "<base_url>/esearch.fcgi?db=<database>&term=<query_column>",
                "<term>&retmode=json"
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
        database = entity(
            name = "E-utils database",
            description = paste0(
                "The name of the E-utils database to search. See ",
                "https://www.ncbi.nlm.nih.gov/books/NBK25501/ for details."
            ),
            value = "gene",
            max_length = 1
        ),
        term = entity(
            name = "E-utils term",
            description = paste0(
                "A correctly formated search term to use with E-utils. See ",
                "https://www.ncbi.nlm.nih.gov/books/NBK25501/ for details. ",
                "When used with the provided url template will automatically ",
                "include the value from the `query_column` at the beginning ",
                "of the term."
            ),
            value = "[pdat]",
            max_length = 1
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
                "200" = .parse_eutils_json,
                "404" = function(...) {
                    return(NULL)
                },
                "400" = function(...) {
                    return(NULL)
                }
            ),
            max_length = Inf
        ),
        result_fields = entity(
            name = "Result field(s)",
            description = paste0(
                "The name of the search result field to return. For E-utils ",
                'this is often "idlist".'
            ),
            value = "idlist",
            max_length = Inf
        )
    )
)
