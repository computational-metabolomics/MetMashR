#' Filter helper function to select records
#'
#' Returns a list of quosures for use with
#' `filter_records` to allow the use of dplyr-style expressions. See examples.
#'
#' @param ... Expressions that return a logical value and are defined in terms
#' of the columns in the annotation_source. If multiple conditions are
#' included then they are combined with the `&` operator. Only records
#' for which all conditions evaluate to `TRUE` are kept.
#'
#' @examples
#' # some annotation data
#' AN <- annotation_source(data = iris)
#'
#' # filter to setosa where Sepal length is less than 5
#' M <- filter_records(
#'     wherever(
#'         Species == "setosa",
#'         Sepal.Length < 5
#'     )
#' )
#' M <- model_apply(M, AN)
#' predicted(M) # 20 rows
#'
#' @returns a list of quosures for use with `filter_records`
#' @seealso [filter_records()]
#' @export
wherever <- function(...) {
    Q <- quos(..., .ignore_empty = "all")
    return(Q)
}

#' @eval get_description('filter_records')
#' @export
#' @include annotation_source_class.R
#' @seealso [dplyr::filter()]
#' @seealso [wherever()]
#' @import rlang
filter_records <- function(where = wherever(A > 0), ...) {
    out <- struct::new_struct(
        "filter_records",
        where = where,
        ...
    )
    return(out)
}


.filter_records <- setClass(
    "filter_records",
    contains = c("model"),
    slots = c(
        updated = "entity",
        where = "entity"
    ),
    prototype = list(
        name = "Filter rows",
        description = paste0(
            "A wrapper around [`dplyr::filter`]. Select rows ",
            "from an annotation table using tidy grammar."
        ),
        type = "filter",
        predicted = "updated",
        .params = c("where"),
        .outputs = c("updated"),
        libraries = c("dplyr", "rlang"),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "The updated annotations as an `annotation_source` object"
            ),
            type = "annotation_source"
        ),
        where = entity(
            name = "Select rows expression",
            description = paste0(
                "A list of [`rlang::quosure`] for evaluation e.g. A>10 will",
                "select all rows where the values in column A are greater than",
                "10. A helper function [`wherever`] is provided to generate",
                "a suitable list of quosures."
            ),
            value = quos(A > 10),
            type = "quosures"
        )
    )
)


#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("filter_records", "annotation_source"),
    definition = function(M, D) {
        q <- M$where

        D$data <- filter(.data = D$data, !!!q)

        M$updated <- D

        return(M)
    }
)
