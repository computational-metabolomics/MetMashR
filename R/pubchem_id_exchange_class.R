#' @eval get_description('pubchem_id_exchange')
#' @export
#' @include annotation_source_class.R
#' @importFrom methods setClass setMethod setGeneric
#' @importFrom xml2 read_xml xml_find_first xml_text xml_attr
#' @importFrom httr2 request req_body_raw req_headers req_perform resp_body_string resp_status
#' @importFrom dplyr left_join %>%
#' @importFrom plyr rbind.fill
#' @family REST API's
#' @details
#' Ported from `structReportsPCB` (same authors), where it was developed to
#' drive PubChem's PUG XML ID Exchange service directly - a batch,
#' asynchronous submit/poll/download workflow, rather than the synchronous
#' per-row PUG REST calls used by [pubchem_compound_lookup()] /
#' [pubchem_property_lookup()]. Prefer this object when translating a large
#' number of identifiers at once, since one request covers many query
#' values instead of one request per row.
pubchem_id_exchange <- function(
        query_column,
        output_type = "inchikey",
        suffix = "_pubchem_id_exchange",
        delay = 2,
        max_attempts = 30,
        max_poll_rounds = 10,
        input_type = "synonyms",
        input_source_name = NULL,
        output_source_name = NULL,
        n_batches = NULL,
        cache = NULL,
        verbose = FALSE,
        ...) {
    if (!is.null(cache)) {
        stopifnot(is_writable(cache))
    }
    out <- struct::new_struct(
        "pubchem_id_exchange",
        query_column = query_column,
        output_type = output_type,
        suffix = suffix,
        delay = delay,
        max_attempts = max_attempts,
        max_poll_rounds = max_poll_rounds,
        input_type = input_type,
        input_source_name = input_source_name,
        output_source_name = output_source_name,
        n_batches = n_batches,
        cache = cache,
        verbose = verbose,
        ...
    )
    return(out)
}

.pubchem_id_exchange <- setClass(
    "pubchem_id_exchange",
    contains = c("model"),
    slots = c(
        query_column = "entity",
        output_type = "enum",
        suffix = "entity",
        delay = "entity",
        max_attempts = "entity",
        max_poll_rounds = "entity",
        input_type = "enum",
        input_source_name = "entity",
        output_source_name = "entity",
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
        name = "PubChem ID Exchange via PUG",
        description = paste0(
            "Uses the PubChem PUG API to perform ID exchange operations. ",
            "Submits a list of identifiers and retrieves corresponding ",
            "identifiers with the same chemical structure. ",
            "Prediction data must contain the column specified by query_column. ",
            "The prediction input need not be the same data used for training, ",
            "but it must contain compatible query identifiers for joining results."
        ),
        type = "PubChem PUG API",
        predicted = "updated",
        .params = c(
            "query_column", "output_type", "suffix", "delay",
            "max_attempts", "max_poll_rounds", "input_type", "input_source_name",
            "output_source_name", "n_batches", "cache", "verbose"
        ),
        .outputs = c("updated", "query_batches", "query_values", "request_ids", "trained"),
        query_column = entity(
            name = "Query column name",
            description = paste0(
                "The name of a column in the annotation table containing values ",
                "to search in the PUG API call."
            ),
            type = "character",
            max_length = 1
        ),
        output_type = enum(
            name = "Output type",
            description = "The type of identifiers to return from the PUG API.",
            allowed = c(
                "regid", "sid", "cid", "inchi", "inchikey",
                "smiles", "synonyms", "title", "iupac"
            ),
            value = "inchikey"
        ),
        suffix = entity(
            name = "Column name suffix",
            description = "A suffix appended to all column names in the returned result.",
            value = "_pubchem_id_exchange",
            type = "character",
            max_length = 1
        ),
        delay = entity(
            name = "Polling delay",
            description = "Delay in seconds between status polling requests.",
            type = c("numeric", "integer"),
            value = 2,
            max_length = 1
        ),
        max_attempts = entity(
            name = "Maximum polling attempts",
            description = "Maximum number of status polling attempts before giving up.",
            type = c("numeric", "integer"),
            value = 30,
            max_length = 1
        ),
        max_poll_rounds = entity(
            name = "Maximum polling rounds",
            description = "Maximum number of whole-batch polling rounds before giving up when some batches are still unavailable.",
            type = c("numeric", "integer"),
            value = 10,
            max_length = 1
        ),
        input_type = enum(
            name = "Input type",
            description = paste0(
                "The type of identifiers being provided as input. ",
                "Determines which XML structure to use for the query."
            ),
            allowed = c(
                "synonyms", "smiles", "inchikey", "inchi",
                "ids", "conformer-ids", "source-ids"
            ),
            value = "synonyms"
        ),
        input_source_name = entity(
            name = "Input source name",
            description = paste0(
                "The name of the external registry source when using Registry IDs ",
                "as input (input_type = 'source-ids'). Required when input_type ",
                "is 'source-ids'."
            ),
            type = c("character", "NULL"),
            value = NULL,
            max_length = 1
        ),
        output_source_name = entity(
            name = "Output source name",
            description = paste0(
                "The name of the external registry source when requesting Registry IDs ",
                "as output (output_type = 'regid'). Required when output_type is 'regid'."
            ),
            type = c("character", "NULL"),
            value = NULL,
            max_length = 1
        ),
        n_batches = entity(
            name = "Number of batches",
            description = paste0(
                "Optional number of POST batches to split the unique query values across. ",
                "If NULL, all query values are submitted in a single request."
            ),
            type = c("numeric", "integer", "NULL"),
            value = NULL,
            max_length = 1
        ),
        cache = entity(
            name = "Cache",
            description = paste0(
                "A struct cache object (e.g. `rds_cache()`) that stores ",
                "parsed responses to previous PubChem ID exchange queries, ",
                "keyed by query value. Values already present in the cache ",
                "are not resubmitted. Besides speeding up re-runs, this ",
                "keeps a local, persistent record of what PubChem returned ",
                "- useful given a translation service can go down or ",
                "change without notice, as happened to the Chemical ",
                "Translation Service this package once depended on (see ",
                "[mwb_refmet_lookup()]). If not using a cache then set to ",
                "NULL."
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
            name = "Stored request IDs",
            description = "PubChem PUG request IDs for each submitted batch.",
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
            description = "Whether the model has already submitted its PubChem jobs.",
            type = "logical",
            value = FALSE,
            max_length = 1
        ),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The annotation_source after adding data returned by the PUG API."
            ),
            type = "annotation_source",
            max_length = Inf
        )
    )
)

.validate_pubchem_id_exchange <- function(M, D = NULL) {
    if (!is.null(D)) {
        if (!(M$query_column %in% colnames(D$data))) {
            stop("query_column is not in the annotation table")
        }
    }

    if (M$output_type == "regid" && is.null(M$output_source_name)) {
        stop("output_source_name is required when output_type is 'regid'")
    }

    if (M$input_type == "source-ids" && is.null(M$input_source_name)) {
        stop("input_source_name is required when input_type is 'source-ids'")
    }

    if (!is.null(M$n_batches)) {
        if (length(M$n_batches) != 1 || is.na(M$n_batches) || !is.numeric(M$n_batches) || M$n_batches < 1) {
            stop("n_batches must be NULL or a single positive integer")
        }
        M$n_batches <- as.integer(M$n_batches)
    }

    if (length(M$max_poll_rounds) != 1 || is.na(M$max_poll_rounds) || !is.numeric(M$max_poll_rounds) || M$max_poll_rounds < 1) {
        stop("max_poll_rounds must be a single positive integer")
    }
    M$max_poll_rounds <- as.integer(M$max_poll_rounds)

    return(M)
}

.split_query_batches <- function(query_values, n_batches = NULL) {
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

.combine_results_and_join <- function(M, D, results_list) {
    non_empty_results <- Filter(function(x) !is.null(x) && nrow(x) > 0, results_list)

    if (length(non_empty_results) > 0) {
        results <- plyr::rbind.fill(non_empty_results)
    } else {
        results <- data.frame(stringsAsFactors = FALSE)
    }

    if (M$verbose) {
        cat("DEBUG: Combined results rows:", nrow(results), "\n")
    }

    # merge with cache: write newly-fetched rows in, then read back the
    # complete set for every value seen during training (cached or not)
    if (!is.null(M$cache)) {
        cached <- read_source(M$cache)$data
        if (!(".search" %in% colnames(cached))) {
            cached <- data.frame(.search = character(0))
        }
        if (nrow(results) > 0) {
            to_cache <- results
            colnames(to_cache)[colnames(to_cache) == M$query_column] <- ".search"
            cached <- unique(plyr::rbind.fill(cached, to_cache))
            if (is_writable(M$cache)) {
                write_database(M$cache, cached)
            } else {
                warning("Cache is not writable and could not be updated.")
            }
        }
        results <- cached[cached$.search %in% M$query_values, , drop = FALSE]
        colnames(results)[colnames(results) == ".search"] <- M$query_column
    }

    if (nrow(results) > 0) {
        # only the query column needs the suffix added here (for the join
        # key below) -- .download_pug_results() already names the output
        # column `<output_type><suffix>`, so suffixing every column here
        # would double it up (e.g. "cid_suffix_suffix")
        colnames(results)[colnames(results) == M$query_column] <-
            paste0(M$query_column, M$suffix)

        by <- paste0(M$query_column, M$suffix)
        names(by) <- M$query_column

        X <- dplyr::left_join(
            D$data,
            results,
            by = by,
            relationship = "many-to-many"
        )
    } else {
        X <- D$data
    }

    D$data <- X
    M$updated <- D
    M
}

.submit_pug_batch <- function(M, query_values) {
    .submit_pug_id_exchange(M, query_values)
}

.poll_pug_batch_status <- function(M, request_id) {
    .poll_pug_status(M, request_id)
}

#' @export
#' @rdname pubchem_id_exchange
setMethod(
    f = "model_train",
    signature = c("pubchem_id_exchange", "annotation_source"),
    definition = function(M, D) {
        M <- .validate_pubchem_id_exchange(M, D)

        if (M$verbose) {
            cat("DEBUG: Starting pubchem_id_exchange model_train\n")
        }

        if (nrow(D$data) == 0) {
            if (M$verbose) {
                cat("DEBUG: No data to train on\n")
            }
            M$trained <- TRUE
            return(M)
        }

        query_values <- unique(D$data[[M$query_column]])
        query_values <- query_values[!is.na(query_values)]
        M$query_values <- query_values

        if (length(query_values) == 0) {
            if (M$verbose) {
                cat("DEBUG: No valid query values for training\n")
            }
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

        query_batches <- .split_query_batches(to_submit, M$n_batches)

        if (M$verbose) {
            cat("DEBUG: Total batches:", length(query_batches), "\n")
            cat("DEBUG: Batch sizes:", paste(vapply(query_batches, length, integer(1)), collapse = ", "), "\n")
        }

        request_ids <- vector("list", length(query_batches))

        for (i in seq_along(query_batches)) {
            if (M$verbose) {
                cat("DEBUG: Submitting batch", i, "of", length(query_batches), "\n")
            }
            request_ids[[i]] <- .submit_pug_batch(M, query_batches[[i]])
        }

        M$query_batches <- query_batches
        M$request_ids <- request_ids
        M$trained <- TRUE

        if (M$verbose) {
            cat("DEBUG: model_train completed\n")
        }

        return(M)
    }
)



#' @export
#' @rdname pubchem_id_exchange
setMethod(
    f = "model_apply",
    signature = c("pubchem_id_exchange", "annotation_source"),
    definition = function(M, D) {
        M = model_train(M,D)
        M = model_predict(M,D)
        return(M)
    }
)

#' @export
#' @rdname pubchem_id_exchange
setMethod(
    f = "model_predict",
    signature = c("pubchem_id_exchange", "annotation_source"),
    definition = function(M, D) {
        M <- .validate_pubchem_id_exchange(M, D)

        if (!isTRUE(M$trained)) {
            stop("Model has not been trained. Run model_train first.")
        }

        if (M$verbose) {
            cat("DEBUG: Starting pubchem_id_exchange model_predict\n")
        }

        if (nrow(D$data) == 0) {
            if (M$verbose) {
                cat("DEBUG: No data to predict on\n")
            }
            M$updated <- D
            return(M)
        }

        if (length(M$request_ids) != length(M$query_batches)) {
            stop("Stored request_ids and query_batches have different lengths")
        }

        if (length(M$request_ids) == 0) {
            # nothing left to submit (query_values were empty, or every
            # value was already in the cache) - go straight to the
            # cache-merge/join step in .combine_results_and_join()
            M <- .combine_results_and_join(M, D, list())
            if (M$verbose) {
                cat("DEBUG: model_predict completed (nothing to fetch)\n")
            }
            return(M)
        }

        download_urls <- vector("list", length(M$request_ids))
        poll_round <- 1L

        repeat {
            not_ready <- integer(0)

            if (M$verbose) {
                cat("DEBUG: Poll round", poll_round, "of", M$max_poll_rounds, "\n")
            }

            for (i in seq_along(M$request_ids)) {
                if (M$verbose) {
                    cat("DEBUG: Polling batch", i, "of", length(M$request_ids), "\n")
                }

                if (
                    is.null(download_urls[[i]]) ||
                    length(download_urls[[i]]) != 1 ||
                    is.na(download_urls[[i]]) ||
                    identical(download_urls[[i]], "")
                ) {
                    download_urls[[i]] <- .poll_pug_batch_status(M, M$request_ids[[i]])
                }

                if (
                    is.null(download_urls[[i]]) ||
                    length(download_urls[[i]]) != 1 ||
                    is.na(download_urls[[i]]) ||
                    identical(download_urls[[i]], "")
                ) {
                    not_ready <- c(not_ready, i)
                }
            }

            if (length(not_ready) == 0) {
                break
            }

            if (poll_round >= M$max_poll_rounds) {
                stop(
                    paste0(
                        "Not all PubChem batches were ready after ",
                        M$max_poll_rounds,
                        " polling rounds. Batches still unavailable: ",
                        paste(not_ready, collapse = ", "),
                        "."
                    )
                )
            }

            if (M$verbose) {
                cat(
                    "DEBUG: Batches not ready yet: ",
                    paste(not_ready, collapse = ", "),
                    ". Sleeping for ", M$delay, " seconds before retry.\n",
                    sep = ""
                )
            }

            Sys.sleep(M$delay)
            poll_round <- poll_round + 1L
        }

        results_list <- vector("list", length(download_urls))

        for (i in seq_along(download_urls)) {
            if (M$verbose) {
                cat("DEBUG: Downloading completed batch", i, "of", length(download_urls), "\n")
            }

            results_list[[i]] <- .download_pug_results(
                M = M,
                download_url = download_urls[[i]],
                query_values = M$query_batches[[i]]
            )
        }

        M <- .combine_results_and_join(M, D, results_list)

        if (M$verbose) {
            cat("DEBUG: model_predict completed\n")
        }

        return(M)
    }
)

.submit_pug_id_exchange <- function(M, query_values) {
    if (M$verbose) {
        cat("DEBUG: Generating XML request for", length(query_values), "values\n")
    }

    xml_request <- .generate_pug_xml(M, query_values)

    if (M$verbose) {
        cat("DEBUG: XML request generated, length:", nchar(xml_request), "characters\n")
    }

    url <- "https://pubchem.ncbi.nlm.nih.gov/pug/pug.cgi"

    if (M$verbose) {
        cat("DEBUG: Submitting POST request to:", url, "\n")
    }

    tryCatch({
        response <- request(url) %>%
            req_body_raw(xml_request, "application/xml") %>%
            req_headers("Content-Type" = "application/xml") %>%
            req_perform()

        if (M$verbose) {
            cat("DEBUG: POST request completed, status:", resp_status(response), "\n")
        }

        content <- resp_body_string(response)
        doc <- xml2::read_xml(content)
        reqid_node <- xml2::xml_find_first(doc, "//PCT-Waiting_reqid")

        if (length(reqid_node) > 0) {
            request_id <- xml2::xml_text(reqid_node)
            if (M$verbose) {
                cat("DEBUG: Successfully extracted request ID:", request_id, "\n")
            }
            return(request_id)
        } else {
            warning("Could not extract request ID from PUG response")
            return(NULL)
        }
    }, error = function(e) {
        warning(paste("Failed to submit PUG request:", e$message))
        return(NULL)
    })
}

.poll_pug_status <- function(M, request_id) {
    if (M$verbose) {
        cat("DEBUG: Checking status for request ID:", request_id, "\n")
    }

    url <- "https://pubchem.ncbi.nlm.nih.gov/pug/pug.cgi"

    status_xml <- paste0(
        '<?xml version="1.0"?>',
        '<!DOCTYPE PCT-Data PUBLIC "-//NCBI//NCBI PCTools/EN" "http://pubchem.ncbi.nlm.nih.gov/pug/pug.dtd">',
        '<PCT-Data>',
        '  <PCT-Data_input>',
        '    <PCT-InputData>',
        '      <PCT-InputData_request>',
        '        <PCT-Request>',
        '          <PCT-Request_reqid>', request_id, '</PCT-Request_reqid>',
        '          <PCT-Request_type value="status"/>',
        '        </PCT-Request>',
        '      </PCT-InputData_request>',
        '    </PCT-InputData>',
        '  </PCT-Data_input>',
        '</PCT-Data>'
    )

    result <- tryCatch({
        response <- request(url) %>%
            req_body_raw(status_xml, "application/xml") %>%
            req_headers("Content-Type" = "application/xml") %>%
            req_perform()

        content <- resp_body_string(response)
        doc <- xml2::read_xml(content)

        status_node <- xml2::xml_find_first(doc, "//PCT-Status")
        if (length(status_node) > 0) {
            status <- xml2::xml_attr(status_node, "value")

            if (M$verbose) {
                cat("DEBUG: Status:", status, "\n")
            }

            if (status == "success") {
                url_node <- xml2::xml_find_first(doc, "//PCT-Download-URL_url")
                if (length(url_node) > 0) {
                    return(xml2::xml_text(url_node))
                }
            } else if (status %in% c("server-error", "input-error", "data-error")) {
                stop(paste("PUG request failed with status:", status))
            }
        }

        return(NA_character_)
    }, error = function(e) {
        stop(paste("Error polling PUG status:", e$message))
    })

    if (is.character(result) && length(result) == 1 && !is.na(result)) {
        return(result)
    }

    return(NULL)
}
.download_pug_results <- function(M, download_url, query_values) {
    download_url <- sub("^ftp", "https", download_url)

    if (M$verbose) {
        cat("DEBUG: Downloading results from:", download_url, "\n")
    }

    tryCatch({
        response <- request(download_url) %>%
            req_perform()

        content <- resp_body_string(response)
        lines <- strsplit(content, "\n")[[1]]
        lines <- lines[lines != ""]

        if (length(lines) == 0) {
            return(data.frame())
        }

        parsed_lines <- strsplit(lines, "\t")
        max_cols <- max(sapply(parsed_lines, length))

        parsed_lines <- lapply(parsed_lines, function(x) {
            if (length(x) < max_cols) {
                c(x, rep("", max_cols - length(x)))
            } else {
                x
            }
        })

        result_df <- as.data.frame(do.call(rbind, parsed_lines), stringsAsFactors = FALSE)

        if (ncol(result_df) >= 2) {
            colnames(result_df) <- c(M$query_column, paste0(M$output_type, M$suffix))
            output_col <- paste0(M$output_type, M$suffix)
            result_df[[output_col]][result_df[[output_col]] == ""] <- NA
        } else if (ncol(result_df) == 1) {
            colnames(result_df) <- M$query_column
        }

        return(result_df)
    }, error = function(e) {
        stop(paste("Failed to download PUG results:", e$message))
    })
}

.generate_pug_xml <- function(M, query_values) {
    if (M$input_type == "source-ids") {
        input_xml <- paste0(
            "                      <PCT-QueryUids_source-ids>",
            "\n                        <PCT-RegistryIDs>",
            "\n                          <PCT-RegistryIDs_source-name>", M$input_source_name, "</PCT-RegistryIDs_source-name>",
            "\n                          <PCT-RegistryIDs_source-ids>",
            paste0(
                "                            <PCT-RegistryIDs_source-ids_E>",
                query_values,
                "</PCT-RegistryIDs_source-ids_E>",
                collapse = "\n"
            ),
            "\n                          </PCT-RegistryIDs_source-ids>",
            "\n                        </PCT-RegistryIDs>",
            "\n                      </PCT-QueryUids_source-ids>"
        )
    } else if (M$input_type == "smiles") {
        input_xml <- paste0(
            "                      <PCT-QueryUids_smiles>",
            paste0(
                "                        <PCT-QueryUids_smiles_E>",
                query_values,
                "</PCT-QueryUids_smiles_E>",
                collapse = "\n"
            ),
            "\n                      </PCT-QueryUids_smiles>"
        )
    } else if (M$input_type == "inchikey") {
        input_xml <- paste0(
            "                      <PCT-QueryUids_inchi-keys>",
            paste0(
                "                        <PCT-QueryUids_inchi-keys_E>",
                query_values,
                "</PCT-QueryUids_inchi-keys_E>",
                collapse = "\n"
            ),
            "\n                      </PCT-QueryUids_inchi-keys>"
        )
    } else if (M$input_type == "inchi") {
        input_xml <- paste0(
            "                      <PCT-QueryUids_inchis>",
            paste0(
                "                        <PCT-QueryUids_inchis_E>",
                query_values,
                "</PCT-QueryUids_inchis_E>",
                collapse = "\n"
            ),
            "\n                      </PCT-QueryUids_inchis>"
        )
    } else if (M$input_type == "ids") {
        input_xml <- paste0(
            "                      <PCT-QueryUids_ids>",
            paste0(
                "                        <PCT-ID-List_E>",
                query_values,
                "</PCT-ID-List_E>",
                collapse = "\n"
            ),
            "\n                      </PCT-QueryUids_ids>"
        )
    } else if (M$input_type == "conformer-ids") {
        input_xml <- paste0(
            "                      <PCT-QueryUids_conformer-ids>",
            paste0(
                "                        <PCT-QueryUids_conformer-ids_E>",
                query_values,
                "</PCT-QueryUids_conformer-ids_E>",
                collapse = "\n"
            ),
            "\n                      </PCT-QueryUids_conformer-ids>"
        )
    } else {
        input_xml <- paste0(
            "                      <PCT-QueryUids_synonyms>",
            paste0(
                "                        <PCT-QueryUids_synonyms_E>",
                query_values,
                "</PCT-QueryUids_synonyms_E>",
                collapse = "\n"
            ),
            "\n                      </PCT-QueryUids_synonyms>"
        )
    }
    xml_request <- paste0(
        '<?xml version="1.0"?>',
        '\n<!DOCTYPE PCT-Data PUBLIC "-//NCBI//NCBI PCTools/EN" "http://pubchem.ncbi.nlm.nih.gov/pug/pug.dtd">',
        '\n<PCT-Data>',
        '\n  <PCT-Data_input>',
        '\n    <PCT-InputData>',
        '\n      <PCT-InputData_query>',
        '\n        <PCT-Query>',
        '\n          <PCT-Query_type>',
        '\n            <PCT-QueryType>',
        '\n              <PCT-QueryType_id-exchange>',
        '\n                <PCT-QueryIDExchange>',
        '\n                  <PCT-QueryIDExchange_input>',
        '\n                    <PCT-QueryUids>',
        input_xml,
        '\n                    </PCT-QueryUids>',
        '\n                  </PCT-QueryIDExchange_input>',
        '\n                  <PCT-QueryIDExchange_operation-type value="same"/>',
        '\n                  <PCT-QueryIDExchange_output-type value="', M$output_type, '"/>',
        if (M$output_type == "regid" && !is.null(M$output_source_name)) {
            paste0('\n                  <PCT-QueryIDExchange_output-dsn>', M$output_source_name, '</PCT-QueryIDExchange_output-dsn>')
        } else {
            ""
        },
        '\n                  <PCT-QueryIDExchange_output-method value="file-pair"/>',
        '\n                  <PCT-QueryIDExchange_compression value="none"/>',
        '\n                </PCT-QueryIDExchange>',
        '\n              </PCT-QueryType_id-exchange>',
        '\n            </PCT-QueryType>',
        '\n          </PCT-Query_type>',
        '\n        </PCT-Query>',
        '\n      </PCT-InputData_query>',
        '\n    </PCT-InputData>',
        '\n  </PCT-Data_input>',
        '\n</PCT-Data>'
    )

    return(xml_request)
}
