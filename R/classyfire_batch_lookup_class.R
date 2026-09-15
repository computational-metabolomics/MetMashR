#' @eval get_description('classyfire_batch_lookup')
#' @export
#' @include annotation_source_class.R
#' @importFrom methods setClass setMethod setGeneric
#' @importFrom httr POST GET content add_headers status_code
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr left_join
#' @family REST API's
#' @details
#' ClassyFire's per-compound endpoint (see [classyfire_lookup()]) only
#' accepts one InChIKey per request and is aggressively rate-limited,
#' making it impractical for more than a few dozen compounds. ClassyFire
#' also provides a batch submission API
#' ([wishartlab/classyfire_api](https://bitbucket.org/wishartlab/classyfire_api/src/master/))
#' - `POST .../queries.json` with many structures at once, then poll
#' `GET .../queries/<id>.json` until finished - which this object uses
#' instead: one submission (or a handful, via `n_batches`) covers the whole
#' input. This batch endpoint takes a **SMILES or InChI** string (not a bare
#' InChIKey) per compound; each is submitted as an `<value><TAB><value>`
#' pair so ClassyFire echoes the value back as its `identifier`, letting
#' results be matched back to the original rows by exact equality rather
#' than relying on ClassyFire's undocumented internal ordering.
classyfire_batch_lookup <- function(
        query_column,
        output_items = c("kingdom", "superclass", "class"),
        output_fields = "name",
        suffix = "_cfb",
        delay = 3,
        max_poll_rounds = 30,
        n_batches = NULL,
        cache = NULL,
        verbose = FALSE,
        ...) {
    if (!is.null(cache)) {
        stopifnot(is_writable(cache))
    }
    out <- struct::new_struct(
        "classyfire_batch_lookup",
        query_column = query_column,
        output_items = output_items,
        output_fields = output_fields,
        suffix = suffix,
        delay = delay,
        max_poll_rounds = max_poll_rounds,
        n_batches = n_batches,
        cache = cache,
        verbose = verbose,
        ...
    )
    return(out)
}

.classyfire_batch_lookup <- setClass(
    "classyfire_batch_lookup",
    contains = c("model"),
    slots = c(
        query_column = "entity",
        output_items = "enum",
        output_fields = "enum",
        suffix = "entity",
        delay = "entity",
        max_poll_rounds = "entity",
        n_batches = "entity",
        cache = "entity",
        verbose = "entity",
        request_ids = "entity",
        query_batches = "entity",
        query_values = "entity",
        trained = "entity",
        updated = "entity"
    ),
    prototype = list(
        name = "ClassyFire batch lookup",
        description = paste0(
            "Uses ClassyFire's batch submission API to obtain chemical ",
            "ontology information (kingdom/superclass/class/...) for many ",
            "SMILES in one or a few requests, rather than one request per ",
            "compound (see [classyfire_lookup()])."
        ),
        type = "classyfire_api",
        predicted = "updated",
        .params = c(
            "query_column", "output_items", "output_fields", "suffix",
            "delay", "max_poll_rounds", "n_batches", "cache", "verbose"
        ),
        .outputs = c("updated", "query_batches", "query_values", "request_ids", "trained"),
        query_column = entity(
            name = "Query column name",
            description = paste0(
                "The name of a column in the annotation table containing ",
                "SMILES strings to submit to ClassyFire."
            ),
            type = "character",
            max_length = 1
        ),
        output_items = enum(
            name = "Output items",
            description = paste0(
                'The names of the items to return: "kingdom", ',
                '"superclass", "class", "subclass" and/or "direct_parent" ',
                "- the taxonomy items ClassyFire returns as a nested ",
                "name/description/chemont_id/url object per compound. ",
                "Keyword \".all\" returns all of them. Other ClassyFire ",
                "fields (substituents, ancestors, ...) are array-valued ",
                "per compound rather than a single nested object and are ",
                "not supported by this object."
            ),
            allowed = c(
                "kingdom", "superclass", "class", "subclass", "direct_parent", ".all"
            ),
            value = c("kingdom", "superclass", "class"),
            max_length = Inf
        ),
        output_fields = enum(
            name = "Output fields",
            description = paste0(
                "The fields to return for each output_item, where ",
                'applicable. Can include "name", "description", ',
                '"chemont_id" and "url". Keyword ".all" returns all fields.'
            ),
            allowed = c("name", "description", "chemont_id", "url", ".all"),
            value = "name",
            max_length = Inf
        ),
        suffix = entity(
            name = "Column name suffix",
            description = "A suffix appended to all column names in the returned result.",
            value = "_cfb",
            type = "character",
            max_length = 1
        ),
        delay = entity(
            name = "Polling delay",
            description = "Delay in seconds between status polling requests.",
            type = c("numeric", "integer"),
            value = 3,
            max_length = 1
        ),
        max_poll_rounds = entity(
            name = "Maximum polling rounds",
            description = "Maximum number of polling rounds before giving up.",
            type = c("numeric", "integer"),
            value = 30,
            max_length = 1
        ),
        n_batches = entity(
            name = "Number of batches",
            description = paste0(
                "Optional number of submissions to split the unique query ",
                "values across. If NULL, all query values are submitted ",
                "in a single request."
            ),
            type = c("numeric", "integer", "NULL"),
            value = NULL,
            max_length = 1
        ),
        cache = entity(
            name = "Cache",
            description = paste0(
                "A struct cache object (e.g. `rds_cache()`) that stores ",
                "parsed responses to previous ClassyFire queries, keyed by ",
                "query value. Values already present in the cache are not ",
                "resubmitted. Besides speeding up re-runs, this keeps a ",
                "local, persistent record of what ClassyFire returned - ",
                "useful given the service can go down or change without ",
                "notice, as happened to the Chemical Translation Service ",
                "this package once depended on (see [mwb_refmet_lookup()]). ",
                "If not using a cache then set to NULL."
            ),
            type = c("annotation_database", "NULL"),
            value = NULL
        ),
        verbose = entity(
            name = "Verbose output",
            description = "Whether to print debug information during execution.",
            type = "logical",
            value = FALSE,
            max_length = 1
        ),
        request_ids = entity(
            name = "Stored request ids",
            description = "ClassyFire query ids for each submitted batch.",
            type = "list",
            value = list(),
            max_length = Inf
        ),
        query_batches = entity(
            name = "Stored query batches",
            description = paste0(
                "The query values submitted in each batch during training ",
                "(excludes any already present in `cache`)."
            ),
            type = "list",
            value = list(),
            max_length = Inf
        ),
        query_values = entity(
            name = "All query values",
            description = paste0(
                "Every unique, non-missing value of `query_column` seen ",
                "during training (cached or not) - used to reconstruct the ",
                "full result set in `model_predict`."
            ),
            type = "character",
            value = character(0),
            max_length = Inf
        ),
        trained = entity(
            name = "Trained flag",
            description = "Whether the model has already submitted its ClassyFire jobs.",
            type = "logical",
            value = FALSE,
            max_length = 1
        ),
        updated = entity(
            name = "Updated annotations",
            description = "The annotation_source after adding data returned by ClassyFire.",
            type = "annotation_source",
            max_length = Inf
        )
    )
)

.validate_classyfire_batch_lookup <- function(M, D = NULL) {
    if (!is.null(D)) {
        if (!(M$query_column %in% colnames(D$data))) {
            stop("query_column is not in the annotation table")
        }
    }
    if (!is.null(M$n_batches)) {
        if (length(M$n_batches) != 1 || is.na(M$n_batches) ||
            !is.numeric(M$n_batches) || M$n_batches < 1) {
            stop("n_batches must be NULL or a single positive integer")
        }
        M$n_batches <- as.integer(M$n_batches)
    }
    return(M)
}

.split_cfb_batches <- function(query_values, n_batches = NULL) {
    if (length(query_values) == 0) {
        return(list())
    }
    if (is.null(n_batches) || n_batches <= 1 || length(query_values) == 1) {
        return(list(query_values))
    }
    n_batches <- min(as.integer(n_batches), length(query_values))
    batch_index <- cut(seq_along(query_values), breaks = n_batches, labels = FALSE)
    split(query_values, batch_index)
}

#' @export
#' @rdname classyfire_batch_lookup
setMethod(
    f = "model_train",
    signature = c("classyfire_batch_lookup", "annotation_source"),
    definition = function(M, D) {
        M <- .validate_classyfire_batch_lookup(M, D)

        query_values <- unique(D$data[[M$query_column]])
        query_values <- query_values[!is.na(query_values) & query_values != ""]
        M$query_values <- query_values

        if (length(query_values) == 0) {
            M$trained <- TRUE
            return(M)
        }

        # skip anything already in the cache
        to_submit <- query_values
        if (!is.null(M$cache)) {
            cached <- read_source(M$cache)$data
            if (".search" %in% colnames(cached)) {
                to_submit <- setdiff(query_values, cached$.search)
            }
        }
        if (M$verbose) {
            cat(
                "DEBUG:", length(query_values) - length(to_submit),
                "of", length(query_values), "query values already cached\n"
            )
        }
        if (length(to_submit) == 0) {
            M$trained <- TRUE
            return(M)
        }

        query_batches <- .split_cfb_batches(to_submit, M$n_batches)
        request_ids <- vector("list", length(query_batches))

        for (i in seq_along(query_batches)) {
            if (M$verbose) {
                cat("DEBUG: submitting ClassyFire batch", i, "of", length(query_batches),
                    "(", length(query_batches[[i]]), "compounds )\n")
            }
            # submit as "<value><TAB><value>" pairs: ClassyFire's batch API
            # (http://bitbucket.org/wishartlab/classyfire_api) accepts an
            # optional identifier before a tab-separated structure, and
            # echoes it back verbatim as each result's `identifier` field -
            # using the query value as its own identifier lets results be
            # matched back by simple equality instead of relying on
            # ClassyFire's undocumented internal ordering.
            query_input <- paste(
                query_batches[[i]], query_batches[[i]],
                sep = "\t", collapse = "\n"
            )
            body <- jsonlite::toJSON(
                list(
                    label = paste0("MetMashR_batch_", i),
                    query_input = query_input,
                    query_type = "STRUCTURE"
                ),
                auto_unbox = TRUE
            )
            resp <- httr::POST(
                "http://classyfire.wishartlab.com/queries.json",
                body = body,
                httr::add_headers("Content-Type" = "application/json")
            )
            httr::stop_for_status(resp)
            parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
            request_ids[[i]] <- parsed$id
        }

        M$query_batches <- query_batches
        M$request_ids <- request_ids
        M$trained <- TRUE
        return(M)
    }
)

#' @export
#' @rdname classyfire_batch_lookup
setMethod(
    f = "model_apply",
    signature = c("classyfire_batch_lookup", "annotation_source"),
    definition = function(M, D) {
        M <- model_train(M, D)
        M <- model_predict(M, D)
        return(M)
    }
)

#' @export
#' @rdname classyfire_batch_lookup
setMethod(
    f = "model_predict",
    signature = c("classyfire_batch_lookup", "annotation_source"),
    definition = function(M, D) {
        M <- .validate_classyfire_batch_lookup(M, D)

        if (!isTRUE(M$trained)) {
            stop("Model has not been trained. Run model_train first.")
        }
        if (nrow(D$data) == 0 || length(M$query_values) == 0) {
            M$updated <- D
            return(M)
        }

        # resolve output_items/output_fields keywords
        output_items <- M$output_items
        if (any(output_items == ".all")) {
            output_items <- c("kingdom", "superclass", "class", "subclass", "direct_parent")
        }
        output_fields <- M$output_fields
        if (any(output_fields == ".all")) {
            output_fields <- c("name", "description", "chemont_id", "url")
        }

        results_list <- vector("list", length(M$request_ids))

        for (i in seq_along(M$request_ids)) {
            qid <- M$request_ids[[i]]
            query_values <- M$query_batches[[i]]

            poll_round <- 1L
            parsed <- NULL
            repeat {
                resp <- httr::GET(paste0(
                    "http://classyfire.wishartlab.com/queries/", qid, ".json"
                ))
                httr::stop_for_status(resp)
                parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))

                if (identical(parsed$classification_status, "Done")) {
                    break
                }
                if (poll_round >= M$max_poll_rounds) {
                    stop(
                        "ClassyFire batch ", i, " not finished after ",
                        M$max_poll_rounds, " polling rounds."
                    )
                }
                if (M$verbose) {
                    cat("DEBUG: batch", i, "status:", parsed$classification_status,
                        "- waiting", M$delay, "s\n")
                }
                Sys.sleep(M$delay)
                poll_round <- poll_round + 1L
            }

            # build a result row per ORIGINAL query value, matched back by
            # exact equality to the identifier we submitted alongside it
            # (see model_train) - entities not found by that identifier
            # (including anything ClassyFire rejected, in invalid_entities)
            # are left as NA
            out <- data.frame(
                query_values,
                stringsAsFactors = FALSE
            )
            colnames(out)[1] <- M$query_column
            for (item in output_items) {
                for (field in output_fields) {
                    out[[paste0(item, ".", field)]] <- NA_character_
                }
            }

            entities <- parsed$entities
            if (is.data.frame(entities) && nrow(entities) > 0) {
                pos_map <- match(entities$identifier, query_values)
                for (j in seq_len(nrow(entities))) {
                    pos <- pos_map[j]
                    if (is.na(pos)) next
                    for (item in output_items) {
                        # jsonlite parses each nested classification object
                        # (kingdom/superclass/class/subclass/direct_parent)
                        # as its own data.frame with one row per entity and
                        # one column per field (name/description/...); other
                        # items (substituents, ancestors, etc.) are
                        # array-valued per entity and not handled here
                        item_col <- entities[[item]]
                        if (is.null(item_col) || !is.data.frame(item_col)) next
                        for (field in output_fields) {
                            if (!(field %in% colnames(item_col))) next
                            fval <- item_col[[field]][j]
                            if (!is.null(fval) && length(fval) == 1 && !is.na(fval)) {
                                out[[paste0(item, ".", field)]][pos] <- fval
                            }
                        }
                    }
                }
            }

            results_list[[i]] <- out
        }

        # results just fetched this run (empty if everything was cached)
        new_results <- if (length(results_list) > 0) {
            plyr::rbind.fill(results_list)
        } else {
            NULL
        }

        # merge with cache: write new rows in, then read back the complete
        # set for every value in M$query_values (cached + just-fetched)
        if (!is.null(M$cache)) {
            cached <- read_source(M$cache)$data
            if (!(".search" %in% colnames(cached))) {
                cached <- data.frame(.search = character(0))
            }
            if (!is.null(new_results)) {
                to_cache <- new_results
                colnames(to_cache)[colnames(to_cache) == M$query_column] <- ".search"
                cached <- unique(plyr::rbind.fill(cached, to_cache))
                if (is_writable(M$cache)) {
                    write_database(M$cache, cached)
                } else {
                    warning("Cache is not writable and could not be updated.")
                }
            }
            collected <- cached[cached$.search %in% M$query_values, , drop = FALSE]
            colnames(collected)[colnames(collected) == ".search"] <- M$query_column
        } else {
            collected <- new_results
        }
        if (is.null(collected)) {
            collected <- data.frame(x = character(0))
            colnames(collected) <- M$query_column
        }

        colnames(collected) <- paste0(colnames(collected), M$suffix)

        by <- paste0(M$query_column, M$suffix)
        names(by) <- M$query_column

        X <- dplyr::left_join(D$data, collected, by = by, relationship = "many-to-many")
        D$data <- X
        M$updated <- D
        return(M)
    }
)
