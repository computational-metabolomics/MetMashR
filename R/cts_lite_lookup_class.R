#' @eval get_description('cts_lite_lookup')
#' @export
#' @include annotation_source_class.R
#' @importFrom methods setClass setMethod
#' @importFrom httr POST content add_headers stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr left_join
#' @family REST API's
#' @details
#' CTS-Lite (the Fiehn Lab's successor to the original, now-closed Chemical
#' Translation Service) matches an InChIKey (or InChI, SMILES, molecular
#' formula or PubChem CID - the query type is auto-detected) against a
#' curated subset of PubChem, returning the matched PubChem entry plus
#' literature/patent annotation counts that can be used as a rough
#' confidence signal. It does not support name-based translation - see
#' [mwb_refmet_lookup()] or [chebi_lookup()] for that. Its REST API
#' (`POST .../match`) is batch-based - every query value is submitted in a
#' single request, rather than one request per value - so, unlike
#' MetMashR's other REST API lookups, this object is not built on the
#' `rest_api` base class.
cts_lite_lookup <- function(
        query_column,
        suffix = "_cts",
        columns = ".all",
        top_hit_only = TRUE,
        first_block_matches = TRUE,
        rdkit_conversion = TRUE,
        cache = NULL,
        cache_mode = "update",
        ...) {
    allowed_columns <- c(
        "query_type", "found_match", "match_level", "pubchem_cid",
        "inchikey", "inchi", "smiles", "compound_name", "molecular_formula",
        "exact_mass", "literature_count", "patent_count",
        "annotation_type_count", ".all"
    )
    check <- all(columns %in% allowed_columns)
    if (!check) {
        w <- which(!(columns %in% allowed_columns))
        stop("Invalid columns: ", paste0(columns[w], collapse = ", "))
    }

    if (!is.null(cache)) {
        stopifnot(is_writable(cache))
    }

    out <- struct::new_struct(
        "cts_lite_lookup",
        query_column = query_column,
        suffix = suffix,
        columns = columns,
        top_hit_only = top_hit_only,
        first_block_matches = first_block_matches,
        rdkit_conversion = rdkit_conversion,
        cache = cache,
        cache_mode = cache_mode,
        ...
    )
    return(out)
}

.cts_lite_lookup <- setClass(
    "cts_lite_lookup",
    contains = c("model"),
    slots = c(
        query_column = "entity",
        suffix = "entity",
        columns = "entity",
        top_hit_only = "entity",
        first_block_matches = "entity",
        rdkit_conversion = "entity",
        base_url = "entity",
        cache = "entity",
        cache_mode = "enum",
        updated = "entity"
    ),
    prototype = list(
        name = "Batch lookup via CTS-Lite",
        description = paste0(
            "Uses the CTS-Lite batch REST API to match InChIKeys against ",
            "a curated subset of PubChem, returning the matched PubChem ",
            "entry plus literature/patent annotation counts."
        ),
        type = "rest_api",
        predicted = "updated",
        .params = c(
            "query_column", "suffix", "columns", "top_hit_only",
            "first_block_matches", "rdkit_conversion", "base_url",
            "cache", "cache_mode"
        ),
        .outputs = c("updated"),
        query_column = entity(
            name = "Query column name",
            description = paste0(
                "The name of a column in the annotation table containing ",
                "InChIKeys (or InChI, SMILES, molecular formula or ",
                "PubChem CID values - CTS-Lite auto-detects the query ",
                "type)."
            ),
            type = "character",
            max_length = 1
        ),
        suffix = entity(
            name = "Column name suffix",
            description = paste0(
                "A suffix appended to all column names in the returned ",
                "result."
            ),
            value = "_cts",
            type = "character",
            max_length = 1
        ),
        columns = entity(
            name = "Columns to return",
            description = paste0(
                'The columns to include in the result. One or more of ',
                '"query_type", "found_match", "match_level", ',
                '"pubchem_cid", "inchikey", "inchi", "smiles", ',
                '"compound_name", "molecular_formula", "exact_mass", ',
                '"literature_count", "patent_count", ',
                '"annotation_type_count". Keyword ".all" (the default) ',
                "returns every column."
            ),
            type = "character",
            value = ".all",
            max_length = Inf
        ),
        top_hit_only = entity(
            name = "Top hit only",
            description = paste0(
                "If TRUE (the default), CTS-Lite returns only the single ",
                "best-ranked match per query (by literature/patent ",
                "count). If FALSE, a query may return multiple matches, ",
                "producing one row per match."
            ),
            type = "logical",
            value = TRUE,
            max_length = 1
        ),
        first_block_matches = entity(
            name = "Allow first-block (skeleton) matches",
            description = paste0(
                "If TRUE (the default), an InChIKey (or converted SMILES) ",
                "query with no exact match may still match on its first ",
                "14 characters (the connectivity/skeleton block), ",
                'reported as match_level = "First Block". If FALSE, only ',
                "exact matches are returned."
            ),
            type = "logical",
            value = TRUE,
            max_length = 1
        ),
        rdkit_conversion = entity(
            name = "RDKit SMILES conversion",
            description = paste0(
                "If TRUE (the default), a SMILES query that fails to ",
                "match directly is converted to an InChIKey with RDKit ",
                "and retried. Has no effect for InChIKey queries."
            ),
            type = "logical",
            value = TRUE,
            max_length = 1
        ),
        base_url = entity(
            name = "Base URL",
            description = "The CTS-Lite match endpoint URL.",
            type = "character",
            value = "https://cts-lite.metabolomics.us/match",
            max_length = 1
        ),
        cache = entity(
            name = "Cache",
            description = paste0(
                "A struct cache object (e.g. `rds_cache()`) that stores ",
                "results of previous CTS-Lite queries, keyed by query ",
                "value. Values already present in the cache are not ",
                "requeried. If not using a cache then set to NULL."
            ),
            type = c("annotation_database", "NULL"),
            value = NULL
        ),
        cache_mode = enum(
            name = "Cache mode",
            description = c(
                "update" = paste0(
                    "The normal mode: values already in `cache` are used ",
                    "as-is, anything missing is queried live and the ",
                    "result added to the cache."
                ),
                "offline" = paste0(
                    "Never query the live CTS-Lite API - only values ",
                    "already present in `cache` are returned, and ",
                    "everything else is left as NA. A warning lists how ",
                    "many query values are not covered by the cache when ",
                    "this happens."
                ),
                "rebuild" = paste0(
                    "Ignore any existing cached value and query the live ",
                    "CTS-Lite API for every value, overwriting the ",
                    "corresponding entry in `cache`."
                )
            ),
            type = "character",
            allowed = c("update", "offline", "rebuild"),
            value = "update",
            max_length = 1
        ),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The annotation_source after adding data returned by ",
                "CTS-Lite."
            ),
            type = "annotation_source",
            max_length = Inf
        )
    )
)

# small helper: return b if a is NULL, otherwise a
.cts_default <- function(a, b) {
    if (is.null(a)) b else a
}

.cts_full_columns <- c(
    "query_type", "found_match", "match_level", "pubchem_cid",
    "inchikey", "inchi", "smiles", "compound_name", "molecular_formula",
    "exact_mass", "literature_count", "patent_count", "annotation_type_count"
)

# parse one query's entry from the CTS-Lite /match response into one row per
# match (or a single all-NA row if found_match is FALSE)
.parse_cts_lite_item <- function(item) {
    base <- list(
        .search = .cts_default(item$query, NA_character_),
        query_type = .cts_default(item$query_type, NA_character_),
        found_match = isTRUE(item$found_match),
        match_level = .cts_default(item$match_level, NA_character_)
    )
    matches <- item$matches
    if (is.null(matches) || length(matches) == 0) {
        return(as.data.frame(c(
            base,
            list(
                pubchem_cid = NA_character_, inchikey = NA_character_,
                inchi = NA_character_, smiles = NA_character_,
                compound_name = NA_character_, molecular_formula = NA_character_,
                exact_mass = NA_real_, literature_count = NA_integer_,
                patent_count = NA_integer_, annotation_type_count = NA_integer_
            )
        ), stringsAsFactors = FALSE))
    }
    rows <- lapply(matches, function(m) {
        as.data.frame(c(
            base,
            list(
                pubchem_cid = .cts_default(m$identifier, NA_character_),
                inchikey = .cts_default(m$inchikey, NA_character_),
                inchi = .cts_default(m$inchi, NA_character_),
                smiles = .cts_default(m$smiles, NA_character_),
                compound_name = .cts_default(m$compound_name, NA_character_),
                molecular_formula = .cts_default(m$molecular_formula, NA_character_),
                exact_mass = as.numeric(.cts_default(m$exact_mass, NA_real_)),
                literature_count = as.integer(.cts_default(m$literature_count, NA_integer_)),
                patent_count = as.integer(.cts_default(m$patent_count, NA_integer_)),
                annotation_type_count = as.integer(
                    .cts_default(m$annotation_type_count, NA_integer_)
                )
            )
        ), stringsAsFactors = FALSE)
    })
    plyr::rbind.fill(rows)
}

#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("cts_lite_lookup", "annotation_source"),
    definition = function(M, D) {
        if (!(M$query_column %in% colnames(D$data))) {
            stop("query_column is not in the annotation table")
        }
        if (nrow(D$data) == 0) {
            M$updated <- D
            return(M)
        }

        query_values <- unique(as.character(D$data[[M$query_column]]))
        query_values <- query_values[!is.na(query_values) & query_values != ""]

        cached <- NULL
        if (!is.null(M$cache)) {
            cached <- read_source(M$cache)$data
            if (!(".search" %in% colnames(cached))) {
                cached <- data.frame(.search = character(0))
            }
        }

        to_query <- query_values
        if (!is.null(cached) && M$cache_mode != "rebuild") {
            to_query <- setdiff(query_values, cached$.search)
        }

        if (length(to_query) > 0 && M$cache_mode == "offline") {
            if (is.null(cached) || nrow(cached) == 0) {
                warning(
                    "cache_mode = 'offline' but the cache is empty or not ",
                    "configured - every value will be left as NA."
                )
            } else {
                warning(
                    length(to_query), " of ", length(query_values),
                    " query values are not in the cache and ",
                    "cache_mode = 'offline' - these will be left as NA ",
                    "rather than queried live."
                )
            }
            to_query <- character(0)
        }

        if (length(to_query) > 0) {
            resp <- httr::POST(
                M$base_url,
                query = list(
                    top_hit_only = tolower(as.character(M$top_hit_only)),
                    first_block_matches = tolower(as.character(M$first_block_matches)),
                    rdkit_conversion = tolower(as.character(M$rdkit_conversion))
                ),
                body = list(queries = paste(to_query, collapse = " ")),
                encode = "json",
                httr::add_headers("Content-Type" = "application/json")
            )
            httr::stop_for_status(resp)
            parsed <- jsonlite::fromJSON(
                httr::content(resp, as = "text", encoding = "UTF-8"),
                simplifyVector = FALSE
            )

            new_rows <- plyr::rbind.fill(lapply(parsed, .parse_cts_lite_item))

            if (!is.null(M$cache)) {
                cached <- cached[!(cached$.search %in% new_rows$.search), , drop = FALSE]
                cached <- unique(plyr::rbind.fill(cached, new_rows))
                if (is_writable(M$cache)) {
                    write_database(M$cache, cached)
                } else {
                    warning("Cache is not writable and could not be updated.")
                }
            } else {
                cached <- new_rows
            }
        }

        if (is.null(cached)) {
            cached <- data.frame(.search = character(0))
        }
        # ensure every expected output column exists (e.g. cache_mode =
        # "offline" against an empty/unpopulated cache would otherwise
        # leave `cached` with only the join key, silently dropping every
        # result column instead of producing NA for them)
        for (col in .cts_full_columns) {
            if (!(col %in% colnames(cached))) {
                cached[[col]] <- rep(NA, nrow(cached))
            }
        }

        df <- cached[cached$.search %in% query_values, , drop = FALSE]

        # subset to the requested columns, always keeping the join key
        cols <- M$columns
        if (!any(cols == ".all")) {
            df <- df[, c(".search", intersect(cols, colnames(df))), drop = FALSE]
        }

        # rename the join key to the raw query_column name, then suffix
        # every column (including that key) in one pass, so the "by"
        # mapping below (query_column + suffix) lines up with what the
        # result columns are actually called
        colnames(df)[colnames(df) == ".search"] <- M$query_column
        colnames(df) <- paste0(colnames(df), M$suffix)

        by <- paste0(M$query_column, M$suffix)
        names(by) <- M$query_column

        X <- D$data
        X[[M$query_column]] <- as.character(X[[M$query_column]])
        X <- dplyr::left_join(X, df, by = by, relationship = "many-to-many")

        D$data <- X
        M$updated <- D
        return(M)
    }
)
