#' @eval get_description('chebi_lookup')
#' @export
#' @include annotation_source_class.R rest_api_class.R chebi_lookup_parsers.R
chebi_lookup <- function(query_column,
    search_by = c("name", "chebi_id"),
    suffix = "_chebi",
    records = "best",
    max_records = "50",
    delay = 1,
    ...) {
    search_by <- match.arg(search_by)

    # the two ChEBI endpoints have different shapes, so the url_template
    # is chosen up front based on the direction of the search
    url_template <- switch(search_by,
        "name" = "<base_url>/es_search?term=<query_column>&size=<max_records>",
        "chebi_id" = "<base_url>/compound/<query_column>"
    )

    out <- struct::new_struct(
        "chebi_lookup",
        query_column = query_column,
        search_by = search_by,
        suffix = suffix,
        records = records,
        max_records = max_records,
        delay = delay,
        url_template = url_template,
        ...
    )

    return(out)
}

.chebi_lookup <- setClass(
    "chebi_lookup",
    contains = c("rest_api"),
    slots = c(
        search_by = "enum",
        records = "enum",
        max_records = "entity"
    ),
    prototype = list(
        name = "ID/synonym lookup via ChEBI",
        description = paste0(
            "Uses the ChEBI REST API to look up a ChEBI identifier from a ",
            "compound name/synonym, or a synonym from a ChEBI identifier, ",
            "based on the input annotation column."
        ),
        type = "rest_api",
        predicted = "updated",
        .params = c("search_by", "records", "max_records"),
        citations = list(
            bibentry(
                bibtype = "article",
                author = as.person(paste0(
                    "Hastings, Janna and Owen, Gareth and Dekker, Adriano ",
                    "and Ennis, Marcus and Kale, Namrata and ",
                    "Muthukrishnan, Venkatesh and Turner, Steve and ",
                    "Swainston, Neil and Mendes, Pedro and Steinbeck, ",
                    "Christoph"
                )),
                title = paste0(
                    "ChEBI in 2016: Improved services and an expanding ",
                    "collection of metabolites"
                ),
                journal = "Nucleic Acids Research",
                volume = 44,
                number = "D1",
                pages = "D1214-D1219",
                year = 2016,
                doi = "10.1093/nar/gkv1031"
            )
        ),
        base_url = entity(
            name = "Base URL",
            description = "The base URL of the API.",
            type = c("character"),
            value = "https://www.ebi.ac.uk/chebi/backend/api/public",
            max_length = 1
        ),
        url_template = entity(
            name = "URL template",
            description = paste0(
                "A template describing how the URL should be ",
                "constructed from the base URL and input parameters. ",
                "Set automatically by the constructor based on ",
                "search_by, so it does not usually need to be set ",
                "directly."
            ),
            value = c(
                "<base_url>/es_search?term=<query_column>&size=<max_records>"
            ),
            max_length = 1
        ),
        query_column = entity(
            name = "Annotation column name",
            description = paste0(
                "The name of a column in the annotation table containing ",
                "the search terms. If search_by = \"name\" this should be ",
                "a column of compound names/synonyms. If search_by = ",
                '"chebi_id" this should be a column of ChEBI identifiers ',
                '(e.g. "CHEBI:27732" or "27732"; any "CHEBI:" prefix is ',
                "stripped automatically before querying the API)."
            ),
            type = c("character"),
            max_length = 1
        ),
        search_by = enum(
            name = "Search by",
            description = c(
                "name" = paste0(
                    "Search ChEBI by compound name/synonym and return ",
                    "the matching ChEBI ID(s)."
                ),
                "chebi_id" = paste0(
                    "Search ChEBI by ChEBI ID and return the matching ",
                    "synonym(s)/name(s)."
                )
            ),
            allowed = c("name", "chebi_id"),
            value = "name"
        ),
        records = enum(
            name = "Returned record(s)",
            description = c(
                paste0(
                    "There can be multiple matches for a given search ",
                    "term (multiple ChEBI entities matching a name, or ",
                    "multiple synonyms for a ChEBI ID)."
                ),
                "best" = paste0(
                    "Return only the single best/primary matching record."
                ),
                "all" = "Return all matching records."
            ),
            allowed = c("best", "all"),
            value = "best"
        ),
        max_records = entity(
            name = "Maximum records fetched",
            description = paste0(
                "The maximum number of hits requested from the ChEBI ",
                "search API when search_by = \"name\" (passed as the ",
                '"size" query parameter). Has no effect when ',
                'search_by = "chebi_id".'
            ),
            type = "character",
            value = "50",
            max_length = 1
        ),
        delay = entity(
            name = "Delay query",
            description = paste0(
                "Delay in seconds between API calls. ChEBI's REST API ",
                "does not publish a specific rate limit, so this ",
                "default is a conservative courtesy value; increase it ",
                "if you see 429/503 responses, or decrease it if you ",
                "have confirmed a higher rate is acceptable."
            ),
            type = c("numeric", "integer"),
            value = 1,
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
                "200" = .parse_chebi_lookup,
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

#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("chebi_lookup", "annotation_source"),
    definition = function(M, D) {
        # ChEBI's REST API expects the bare numeric part of a ChEBI
        # accession (e.g. "27732") in the URL path when searching by
        # chebi_id, not the full "CHEBI:27732" accession string. A
        # temporary column with any "CHEBI:" prefix stripped is used for
        # the query so the original annotation column is left untouched.
        if (identical(M$search_by, "chebi_id")) {
            temp_col <- paste0(".", M$query_column, "_numeric")

            D2 <- D
            D2$data[[temp_col]] <- gsub(
                "^\\s*CHEBI:\\s*", "",
                as.character(D$data[[M$query_column]]),
                ignore.case = TRUE
            )

            original_query_column <- M$query_column
            M$query_column <- temp_col

            M <- callNextMethod(M, D2)

            # drop the temporary column and restore the original
            # query_column name now that querying is finished
            M$updated$data[[temp_col]] <- NULL
            M$query_column <- original_query_column
        } else {
            M <- callNextMethod(M, D)
        }

        return(M)
    }
)
