#' @eval get_description('pubchem_widget')
#' @export
pubchem_widget <- function(query_column,
                           row_index,
                           record_type = "2D-Structure",
                           hide_title = FALSE,
                           width = "600px",
                           height = "650px",
                           display = TRUE,
                           ...) {
    out <- struct::new_struct(
        "pubchem_widget",
        query_column = query_column,
        row_index = row_index,
        record_type = record_type,
        hide_title = hide_title,
        width = width,
        height = height,
        display = display,
        ...
    )

    return(out)
}


.pubchem_widget <- setClass(
    "pubchem_widget",
    contains = "chart",
    slots = c(
        query_column = "entity",
        search_by = "enum",
        row_index = "entity",
        record_type = "entity",
        hide_title = "entity",
        width = "entity",
        height = "entity",
        display = "entity"
    ),
    prototype = list(
        name = "PubChem widget",
        description = paste0(
            "Display a PubChem HTML widget for a compound."
        ),
        type = "hmltwidget",
        .params = c(
            "query_column", "row_index", "record_type",
            "hide_title", "width", "height", "display"
        ),
        query_column = entity(
            name = "PubChem query column",
            value = "V1",
            type = c("character"),
            description = paste0(
                "The name of the `annotation_source` column with compound ",
                "identifiers of the type specified in the `search_by` param."
            )
        ),
        libraries = "htmltools",
        row_index = entity(
            name = "Row index",
            description = paste0(
                "The row index of the `annotation_source` to request an image ",
                "of the molecular structure of."),
            type = c("integer", "numeric"),
            value = 1
        ),
        record_type = entity(
            name = "Record type",
            description = paste0(
                "The record type for the widget."
            ),
            type = "character",
            value = "2D-Structure",
            max_length = 1
        ),
        hide_title = entity(
            name = "Hide widget title",
            description = c(
                "TRUE" = "The title is displayed.",
                "FALSE" = "The title is not displayed."
            ),
            type = "logical",
            value = FALSE,
            max_length = 1
        ),
        width = entity(
            name = "Widget width",
            description = paste0(
                'The width of the widget in a CSS style compatible format. ',
                'Numerical values will be converted to character.'
            ),
            type = c("integer", "numeric", "character"),
            value = 600,
            max_length = 1
        ),
        height = entity(
            name = "Widget height",
            description = paste0(
                'The height of the widget in a CSS style compatible format.',
                'Numerical values will be converted to character.'
            ),
            type = c("integer", "numeric", "character"),
            value = 400,
            max_length = 1
        ),
        display = entity(
            name = "Display widget",
            description = c(
                "TRUE" = "Display the widget.",
                "FALSE" = "Do not display the widget and only return the HTML."
            ),
            type = c("logical"),
            value = TRUE,
            max_length = 1
        )
    )
)

#' @export
setMethod(
    f = "chart_plot",
    signature = c("pubchem_widget", "annotation_source"),
    definition = function(obj, dobj) {
        # get row
        dobj$data <- dobj$data[obj$row_index, ]

        # handle hide_title, as hide_title=false doesnt work
        ht <- ""
        if (obj$hide_title) {
            ht <- paste0(
                "&hide_title=",
                tolower(as.character(obj$hide_title))
            )
        }

        # construct html
        html <- paste0(
            '<iframe class="pubchem-widget" src=',
            '"https://pubchem.ncbi.nlm.nih.gov/compound/',
            dobj$data[[obj$query_column]][1],
            "#section=",
            obj$record_type,
            ht,
            '&embed=true" ',
            'style="width: ', obj$width, "; ",
            "height: ", obj$height, "; ",
            'border: 0;">',
            "</iframe>"
        )

        # plot
        H <- htmltools::HTML(html)
        if (obj$display) {
            htmltools::html_print(H)
        }
        return(H)
    }
)
