#' @eval get_description('normalise_strings')
#' @export
#' @include annotation_source_class.R
#' @details Each item of the `dictionary` list should #' have at least two
#' fields: "pattern" and "replace". "pattern" is used as
#' inputs to the `[grepl()]` function to detect matches to the input pattern.
#' Parameters such as `perl = TRUE` can also be included in the list and these
#' will be passed to `[grepl()]`, otherwise the defaults are used.
#' When a match is detected the function in "replace" is called with the same
#' inputs as `[grepl()]`. The "replace" function should return a new string.
#' Alternatively `replace = NA` can be used to return NA for a matching pattern.
#' If a character string is provided then `[gsub()]` will be used by default.
#' @seealso [grepl()], [gsub()]
normalise_strings <- function(search_column,
                              output_column = NULL,
                              dictionary = list(),
                              ...) {
    out <- struct::new_struct(
        "normalise_strings",
        search_column = search_column,
        output_column = output_column,
        dictionary = dictionary,
        ...
    )

    return(out)
}


.normalise_strings <- setClass(
    "normalise_strings",
    contains = c("model"),
    slots = c(
        updated = "entity",
        search_column = "entity",
        output_column = "entity",
        dictionary = "entity"
    ),
    prototype = list(
        name = "Normalise string",
        description = paste0(
            "Replace matching (sub)strings ",
            "based on a provided dictionary of search ",
            "terms and their replacements."
        ),
        type = "filter",
        predicted = "updated",
        .params = c("search_column", "output_column", "dictionary"),
        .outputs = c("updated"),
        libraries = "dplyr",
        search_column = entity(
            name = "Search column",
            description = paste0(
                "The column name of the input `annotation_source` that will be",
                " searched for matching (sub)strings."
            ),
            type = "character",
            max_length = 1
        ),
        output_column = entity(
            name = "Output column",
            description = paste0(
                "The name of a new column that the modified strings will be ",
                "stored in. If NULL the `search_column` will be replaced."
            ),
            type = c("character", "NULL"),
            max_length = 1
        ),
        dictionary = entity(
            name = "Dictionary",
            description = paste0(
                "A list of patterns and functions that take the input pattern ",
                "and return a replacement string. ",
                "A `annotation_database` object ",
                "containing a suitable list can also be used here."
            ),
            type = c("list", "annotation_database")
        ),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The updated annotations as an `annotation_source` object"
            ),
            type = "annotation_source"
        )
    )
)


#' @export
setMethod(
    f = "model_apply",
    signature = c("normalise_strings", "annotation_source"),
    definition = function(M, D) {
        # import database if provided
        if (is(M$dictionary, "annotation_database")) {
            dict <- read_source(M$dictionary)
        } else {
            # assume list
            dict <- M$dictionary
        }

        # for each string in the search column
        fixed <- D$data[[M$search_column]] # store result here
        for (s in seq(from = 1, to = length(fixed))) {
            # for each term in the dictionary
            for (p in seq(from = 1, to = length(dict))) {
                # replacement function
                fun <- dict[[p]]$replace

                # prep grepl inputs
                inputs <- dict[[p]]
                inputs$replace <- NULL
                inputs$x <- fixed[s]

                if (do.call(grepl, inputs)) {
                    if (is(fun, "function")) {
                        # use function to replace match
                        inputs$replacement <- fun
                        fixed[s] <- fun(inputs)
                    } else if (is.na(fun)) {
                        # replace with NA
                        fixed[s] <- fun
                    } else if (is.character(fun)) {
                        # gsub
                        inputs$replacement <- fun
                        fixed[s] <- do.call(gsub, inputs)
                    } else {
                        # throw error
                        stop(
                            "unable to use replacement for ",
                            "dictionary index ", p
                        )
                    }
                }
            }
        }

        if (is.null(M$output_column)) {
            D$data[[M$search_column]] <- fixed
        } else {
            # if output column exists warn
            if (M$output_column %in% colnames(D$data)) {
                warning(
                    "The output column already exists and will be ",
                    "replaced."
                )
            }
            D$data[[M$output_column]] <- fixed
        }

        M$updated <- D

        return(M)
    }
)
