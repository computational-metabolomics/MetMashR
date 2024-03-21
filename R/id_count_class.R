#' @eval get_description('id_counts')
#' @export
#' @include annotation_source_class.R
id_counts <- function(id_column,
    count_column = "id_counts",
    count_na = TRUE,
    ...) {
    out <- struct::new_struct("id_counts",
        id_column = id_column,
        count_column = count_column,
        count_na = count_na,
        ...
    )
    return(out)
}



.id_counts <- setClass(
    "id_counts",
    contains = c("model"),
    slots = c(
        id_column = "entity",
        count_column = "entity",
        updated = "entity",
        count_na = "entity"
    ),
    prototype = list(
        name = "id counts",
        description = paste0(
            "Adds the number of times an identical identifier is present to ",
            "each record."
        ),
        type = "univariate",
        predicted = "updated",
        .params = c("id_column", "count_column", "count_na"),
        .outputs = c("updated"),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The input annotation source with the newly ",
                "generated column."
            ),
            type = "annotation_source"
        ),
        count_column = entity(
            name = "count column name",
            description = "The name of the new column to store the counts in.",
            type = "character"
        ),
        id_column = entity(
            name = "id column name",
            description = "column name of the variable ids in variable_meta.",
            type = "character"
        ),
        count_na = entity(
            name = "Count NA",
            description = c(
                "TRUE" = "Report number of NA",
                "FALSE" = "Do not report number of NA"
            ),
            value = TRUE,
            max_length = 1,
            type = "logical"
        )
    )
)


#' @export
setMethod(
    f = "model_apply",
    signature = c("id_counts", "annotation_source"),
    definition = function(M, D) {
        X <- D$data

        # if no rows add count column then return
        if (nrow(X) == 0) {
            X[[M$count_column]] <- numeric(0)
            D$data <- X
            M$updated <- D
            return(M)
        }

        # for each record
        X[[M$count_column]] <- 0 # prefill with zero
        for (k in seq_len(nrow(X))) {
            # get the identifier
            id <- X[[M$id_column]][k]

            if (is.na(id)) {
                if (M$count_na) {
                    # number of NA
                    n <- sum(is.na(X[[M$id_column]]))
                } else {
                    n <- NA
                }
            } else {
                # number of occurrences
                n <- sum(X[[M$id_column]] == id, na.rm = TRUE)
            }
            # update annotations
            X[[M$count_column]][k] <- n
        }
        D$data <- X
        M$updated <- D

        return(M)
    }
)
