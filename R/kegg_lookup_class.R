#' @eval get_description('kegg_lookup')
#' @export
#' @include annotation_source_class.R
#' @family REST API's
kegg_lookup <- function(get = "pubchem",
    from = "compound",
    query_column,
    suffix = "_kegg",
    cache = NULL,
    cache_mode = "update",
    ...) {
    # check for suitable combinations of get and from
    if (get %in% c("compound", "drug", "glycan") &
        from %in% c("compound", "drug", "glycan")) {
        stop(
            "compound, drug and glycan ids can only be converted to chebi ",
            "or pubchem ids"
        )
    }
    if (get %in% c("pubchem", "chebi") &
        from %in% c("pubchem", "chebi")) {
        stop(
            "pubchem and chebi ids can only be converted to compound, drug ",
            "or glycan ids"
        )
    }

    if (!is.null(cache)) {
        stopifnot(is_writable(cache))
    }

    out <- struct::new_struct(
        "kegg_lookup",
        get = get,
        from = from,
        query_column = query_column,
        suffix = suffix,
        cache = cache,
        cache_mode = cache_mode,
        ...
    )
    return(out)
}

.kegg_lookup <- setClass(
    "kegg_lookup",
    contains = c("model"),
    slots = c(
        get = "enum",
        from = "enum",
        query_column = "entity",
        updated = "entity",
        suffix = "entity",
        cache = "entity",
        cache_mode = "enum"
    ),
    prototype = list(
        name = "Convert to or from kegg identifiers",
        description = paste0(
            "Searches the Kegg database to obtain external ",
            "identifiers. KEGG compound, drug and glycan databases can be ",
            "queried for pubchem and chebi identifiers, and vice-versa."
        ),
        type = "kegg_api",
        predicted = "updated",
        libraries = c("KEGGREST", "dplyr"),
        .params = c("get", "from", "query_column", "suffix", "cache", "cache_mode"),
        .outputs = c("updated"),
        query_column = entity(
            name = "From column name",
            description = paste0(
                "The name of the column containing ",
                "identifiers to search the database for. They should be ",
                'identifiers of the type selected for the "from" slot.'
            ),
            type = c("character"),
            value = "V1",
            max_length = 1
        ),
        get = enum(
            name = "Get identifier",
            description = c(
                "compound" = "KEGG small molecule database",
                "glycan" = "KEGG glycan database",
                "drug" = "KEGG drug database",
                "chebi" = paste0(
                    "Chemical Entities of Biological Interest (ChEBI) database"
                ),
                "pubchem" = "PubChem Substance Identifier"
            ),
            type = "character",
            max_length = 1,
            allowed = c("compound", "glycan", "drug", "chebi", "pubchem"),
            value = "pubchem"
        ),
        from = enum(
            name = "From identifier",
            description = c(
                "compound" = "KEGG small molecule database",
                "glycan" = "KEGG glycan database",
                "drug" = "KEGG drug database",
                "chebi" = paste0(
                    "Chemical Entities of Biological Interest (ChEBI) database"
                ),
                "pubchem" = "PubChem Substance Identifier"
            ),
            type = "character",
            max_length = 1,
            allowed = c("compound", "glycan", "drug", "chebi", "pubchem"),
            value = "compound"
        ),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "An annotation_source object with a new ",
                "column of compound identifiers"
            ),
            type = "annotation_source",
            max_length = Inf
        ),
        suffix = entity(
            name = "Column name suffix",
            description = paste0(
                "A suffix appended to all column names in ",
                "the returned result."
            ),
            value = "_kegg",
            type = "character",
            max_length = 1
        ),
        cache = entity(
            name = "Cache",
            description = paste0(
                "A struct cache object (e.g. `rds_cache()`) that stores ",
                "results of previous KEGG conversions, keyed by query ",
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
                    "Never query the live KEGG API - only values already ",
                    "present in `cache` are returned, and everything else ",
                    "is left as NA. Useful for continuing to work with a ",
                    "partially-populated cache while KEGG is unreachable ",
                    "or down, without waiting on or erroring against the ",
                    "live service. A warning lists how many query values ",
                    "are not covered by the cache when this happens (and, ",
                    "if the cache is entirely empty or unset, that every ",
                    "value will be returned as NA)."
                ),
                "rebuild" = paste0(
                    "Ignore any existing cached value and query the live ",
                    "KEGG API for every value, overwriting the ",
                    "corresponding entry in `cache`. Useful when cached ",
                    "results are known to be stale."
                )
            ),
            type = "character",
            allowed = c("update", "offline", "rebuild"),
            value = "update",
            max_length = 1
        )
    )
)


#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("kegg_lookup", "annotation_source"),
    definition = function(M, D) {
        # check for 0 annotations
        if (nrow(D$data) == 0) {
            # add column
            D$data[[paste0(M$get, M$suffix)]] <- character(0)
            # nothing to do, so return
            M$updated <- D
            return(M)
        }

        out_col <- paste0(M$get, M$suffix)

        # get source column
        src <- as.character(D$data[[M$query_column]])
        query_values <- unique(src[!is.na(src)])

        # read cache, if used
        cached <- NULL
        if (!is.null(M$cache)) {
            cached <- read_source(M$cache)$data
            if (!(".search" %in% colnames(cached))) {
                cached <- data.frame(.search = character(0))
            }
        }

        # only query values not already cached ("rebuild" mode ignores the
        # cache here and re-queries every value)
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
            # add from str
            src_str <- paste(M$from, to_query, sep = ":")

            # query kegg
            result <- KEGGREST::keggConv(
                target = M$get,
                source = src_str,
                querySize = 100
            )

            # convert to data.frame
            df <- data.frame(from = names(result), get = result)

            # extract ids
            df <- lapply(df, function(x) {
                y <- strsplit(x, ":", fixed = TRUE)
                y <- unlist(lapply(y, "[", i = 2))
                return(y)
            })

            df <- as.data.frame(df)

            if (nrow(df) == 0) {
                df <- data.frame(from = character(0), get = character(0))
            }

            colnames(df) <- c(".search", out_col)

            if (!is.null(M$cache)) {
                # drop any stale entry for values being (re-)written first
                # (relevant in "rebuild" mode, a no-op otherwise)
                cached <- cached[!(cached$.search %in% df$.search), , drop = FALSE]
                cached <- unique(plyr::rbind.fill(cached, df))
                if (is_writable(M$cache)) {
                    write_database(M$cache, cached)
                } else {
                    warning("Cache is not writable and could not be updated.")
                }
            } else {
                cached <- df
            }
        }

        if (is.null(cached)) {
            cached <- data.frame(.search = character(0))
            cached[[out_col]] <- character(0)
        }
        # cache may be non-NULL but still missing out_col entirely (e.g.
        # a brand new/empty cache with cache_mode = "offline", which never
        # populates it) - without this, the join below would silently drop
        # the output column instead of producing NA for every row
        if (!(out_col %in% colnames(cached))) {
            cached[[out_col]] <- rep(NA_character_, nrow(cached))
        }

        # results for every query value seen (cached or freshly fetched)
        df <- cached[cached$.search %in% query_values, , drop = FALSE]
        colnames(df)[colnames(df) == ".search"] <- paste0(M$query_column, M$suffix)
        by <- paste0(M$query_column, M$suffix)
        names(by) <- M$query_column

        # left join with annotations (keggConv excludes ids with no hit)
        X <- D$data
        X[[M$query_column]] <- as.character(X[[M$query_column]])

        X <- dplyr::left_join(X, df, by = by)

        # update
        D$data <- X
        M$updated <- D

        # return
        return(M)
    }
)
