#' @eval get_description('rest_api')
#' @export
#' @include zzz.R rest_api_parsers.R
#' @import httr
#' @family {REST API's}
rest_api <- function(base_url,
                     url_template,
                     suffix,
                     status_codes,
                     delay,
                     cache = NULL,
                     query_column,
                     ...) {
    # check supplied cache is writable
    if (!is.null(cache)) {
        stopifnot(is_writable(cache))
    }

    out <- struct::new_struct(
        "rest_api",
        base_url = base_url,
        url_template = url_template,
        suffix = suffix,
        status_codes = status_codes,
        delay = delay,
        cache = cache,
        query_column = query_column,
        ...
    )
    return(out)
}

.rest_api <- setClass(
    "rest_api",
    contains = c("model"),
    slots = c(
        base_url = "entity",
        url_template = "entity",
        query_column = "entity",
        cache = "entity",
        status_codes = "entity",
        parse_response = "entity",
        updated = "entity",
        delay = "entity",
        suffix = "entity",
        .encode_reserved = "logical"
    ),
    prototype = list(
        name = "rest_api",
        description = paste0("
            A base class providing common methods for making REST API calls."),
        type = "REST API",
        predicted = "updated",
        .params = c(
            "base_url", "url_template", "query_column",
            "cache", "status_codes", "delay", "suffix"
        ),
        .outputs = c("updated"),
        base_url = entity(
            name = "Base URL",
            description = "The base URL of the API.",
            type = c("character"),
            value = "V1",
            max_length = 1
        ),
        cache = entity(
            name = "Cache",
            description = paste0(
                "A struct cache object that contains ",
                "parsed responses to previous api queries. If not using a ",
                "cache then set to NULL."
            ),
            type = c("annotation_database", "NULL"),
            value = NULL
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
            max_length = Inf
        ),
        query_column = entity(
            name = "Query column name",
            description = paste0(
                "The name of a column in the annotation ",
                "table containing values to search in the api call."
            ),
            type = "character",
            max_length = Inf
        ),
        suffix = entity(
            name = "Column name suffix",
            description = paste0(
                "A suffix appended to all column names in ",
                "the returned result."
            ),
            value = "_rest_api",
            type = "character",
            max_length = 1
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
                "200" = function(response, ...) {
                    return(content(response))
                },
                "404" = function(...) {
                    return(NULL)
                }
            ),
            max_length = Inf
        ),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The annotation_source after adding data ",
                "returned by the API."
            ),
            type = "annotation_source",
            max_length = Inf
        ),
        delay = entity(
            name = "Delay query",
            description = "Delay in seconds between API calls.",
            type = c("numeric", "integer"),
            value = 0.5,
            max_length = 1
        ),
        .encode_reserved = TRUE
    )
)


#' @export
setMethod(
    f = "model_apply",
    signature = c("rest_api", "annotation_source"),
    definition = function(M, D) {
        # check query column is in annotation table
        check <- M$query_column %in% colnames(D$data)
        if (!check) {
            stop("query_column is not in the annotation table")
        }


        # check for 0 annotations
        if (nrow(D$data) == 0) {
            # nothing to do, so return
            M$updated <- D
            return(M)
        }

        # import cache
        cached <- NULL
        if (!is.null(M$cache)) {
            # read source
            cached <- read_source(M$cache)
            # if empty, create a compatible one, otherwise use loaded
            if (ncol(cached$data) == 0) { # probably new source
                cached <- data.frame(.search = character(0))
            } else {
                cached <- cached$data
            }

            # must have a search column
            if (!(".search" %in% colnames(cached))) {
                stop('Cache does not contain a ".search" column')
            }
        }




        collected <- list()
        # for each query term
        for (k in D$data[[M$query_column]]) {
            if (is.na(k)) {
                # if NA then don't submit query
                next
            }

            # build url
            u <- .build_api_url(M, query_column = as.character(k))

            # check cache if used
            parsed <- NULL
            if (!is.null(cached)) {
                # filter on query column
                parsed <-
                    cached %>%
                    filter(.data[[".search"]] == k)

                # if no hits, reset to null
                if (nrow(parsed) == 0) {
                    parsed <- NULL
                } else {
                    # rename search column
                    colnames(parsed)[colnames(parsed) == ".search"] <-
                        M$query_column
                }
            }

            # if not using cache, or no hits in cache
            if (is.null(parsed)) {
                # delay
                Sys.sleep(M$delay)

                # submit query api
                parsed <- .submit_api_query(
                    URL = u,
                    FUN = .submit_fun,
                    params = param_list(M)
                )

                # if we dont get a data.frame then create
                if (!is.data.frame(parsed)) {
                    parsed <- list()
                    parsed[[M$query_column]] <- k
                    parsed <- as.data.frame(parsed)
                }
                # if result has 0 rows then create
                if (nrow(parsed) == 0) {
                    parsed <- list()
                    parsed[[M$query_column]] <- k
                    parsed <- as.data.frame(parsed)
                }

                # make sure the search term is included in the data.frame
                parsed[[M$query_column]] <- k

                # update cached (even if not saving cache,
                # to avoid same query multiple times)
                forcache <- parsed
                colnames(forcache)[colnames(parsed) == M$query_column] <-
                    ".search"
                cached <- plyr::rbind.fill(cached, forcache)

                if (!is.null(M$cache)) {
                    # keep unique records
                    cached <- unique(cached)
                    # write to cache
                    if (is_writable(M$cache)) {
                        write_database(M$cache, cached)
                    } else {
                        warning(
                            "Cache is not writable and could not be updated.")
                    }
                }
            }

            # collect results
            collected[[k]] <- parsed
        }
        # join results, pad missing columns with NA to get all columns
        collected <- plyr::rbind.fill(collected)

        # update cache if using
        if (!is.null(M$cache)) {
            # keep unique records
            cached <- unique(cached)
            # write to cache
            if (is_writable(M$cache)) {
                write_database(M$cache, cached)
            } else {
                warning("Cache is not writable and could not be updated.")
            }
        }

        # add suffix
        colnames(collected) <- paste0(colnames(collected), M$suffix)

        # join with annotations
        by <- paste0(M$query_column, M$suffix)
        names(by) <- M$query_column
        X <- dplyr::left_join(D$data, collected,
            by = by,
            relationship = "many-to-many"
        )

        # update object
        D$data <- X
        M$updated <- D

        # return
        return(M)
    }
)

# internal function to construct url from template
.build_api_url <- function(M, ...) {
    # get template
    template <- M$url_template

    # get params between <>
    found <- regmatches(
        template,
        gregexpr("(?<=\\<)[^<>]+(?=\\>)",
            template,
            perl = TRUE
        )
    )[[1]]

    # get list of params and values
    L <- param_list(M)

    # replace search terms
    IN <- list(...)
    for (n in names(IN)) {
        L[[n]] <- IN[[n]]
    }

    # for each param, replace it in the template with its value in the object
    for (k in found) {
        # only encode if not base_url
        if (k == "base_url") {
            r <- L[[k]]
        } else {
            r <- URLencode(L[[k]], reserved = M@.encode_reserved)
        }
        template <- sub(
            pattern = paste0("<", k, ">"),
            replacement = r,
            x = template, fixed = TRUE
        )
    }

    return(template)
}

# internal function to submit an api query with a number of attempts and
# and return an error if fails
.submit_api_query <- function(URL, FUN, N.TRIES = 1L, params = NULL) {
    # modified from
    # https://contributions.bioconductor.org/querying-web-resources.html
    N.TRIES <- as.integer(N.TRIES)
    stopifnot(length(N.TRIES) == 1L, !is.na(N.TRIES))

    while (N.TRIES > 0L) {
        result <- tryCatch(
            FUN(
                URL = URL,
                params = params
            ),
            error = identity
        )

        if (!inherits(result, "error")) {
            break
        } else {
            N.TRIES <- N.TRIES - 1L
        }
    }

    if (N.TRIES == 0L) {
        stop(
            "'getURL()' failed:",
            "\n  URL: ", URL,
            "\n  error: ", conditionMessage(result)
        )
    }

    result
}

# internal function to submit an api query
.submit_fun <- function(URL, params = NULL) {
    response <- GET(URL, timeout(getOption("timeout")))

    # get status code
    s <- as.character(status_code(response))

    # respond as specified
    if (s %in% names(params$status_codes)) {
        out <- params$status_codes[[s]](response, params)
        return(out)
    }

    # otherwise check for status
    stop_for_status(response)
}
