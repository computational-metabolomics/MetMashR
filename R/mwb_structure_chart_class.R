#' @eval get_description('mwb_structure')
#' @details This object queries the Metabolomics Workbench API for matches to
#' your query without caching the results. It is therefore intended for limited
#' use. If you wish to obtain images for a large number of molecules you should
#' seek an alternative solution.
#' @export
mwb_structure <- function(
        query_column,
        row_index,
        ...) {
    out <- struct::new_struct(
        "mwb_structure",
        query_column = query_column,
        row_index = row_index,
        ...
    )
    
    return(out)
}


.mwb_structure <- setClass(
    "mwb_structure",
    contains = "chart",
    slots = c(
        row_index = "entity",
        query_column = "entity"
    ),
    prototype = list(
        name = "MWB molecular structure",
        description = paste0(
            "Query the Metabolomic Workbench API and retrieve a display an ",
            "image of the matching molecular structure."
        ),
        type = "image",
        .params = c("row_index", "query_column"),
        libraries = c("cowplot", "metabolomicsWorkbenchR"),
        row_index = entity(
            name = "Row index",
            description = paste0(
                "The row index of the `annotation_source` to request an image ",
                "of the molecular structure of."
            ),
            type = c("integer", "numeric"),
            value = 1
        ),
        query_column = entity(
            name = "Query column",
            value = "V1",
            type = c("character"),
            description = paste0(
                "The name of the `annotation_source` column with regno ",
                "compound identifiers"
            )
        )
    )
)

#' @export
#' @template chart_plot
setMethod(
    f = "chart_plot",
    signature = c("mwb_structure", "annotation_source"),
    definition = function(obj, dobj) {
        # get row
        dobj$data <- dobj$data[obj$row_index, ]
        
        # use api
        P <- mwb_compound_lookup(
            input_item = "regno",
            query_column = obj$query_column,
            output_item = "png",
            suffix = "_.mwb_img",
            status_codes = list(
                "200" = function(response, ...) {
                    return(data.frame(png = I(list(content(response)))))
                }
            )
        )
        
        P <- model_apply(P, dobj)
        img <- P$updated$data[["png_.mwb_img"]][[1]]
        g <- cowplot::ggdraw() + cowplot::draw_image(img, scale = 0.95)
        return(g)
    }
)
