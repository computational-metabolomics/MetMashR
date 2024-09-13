#' @eval get_description('pubchem_structure')
#' @details This object queries the PubChem API for matches to your query
#' without caching the results. It is therefore intended for limited use. If
#' you wish to obtain images for a large number of moelucules you should seek
#' an alternative solution.
#' @export
pubchem_structure <- function(
        query_column,
        search_by,
        row_index,
        record_type = "2d",
        image_size = "large",
        ...) {
    out <- struct::new_struct(
        "pubchem_structure",
        query_column = query_column,
        search_by = search_by,
        row_index = row_index,
        record_type = record_type,
        image_size = image_size,
        ...
    )
    
    return(out)
}


.pubchem_structure <- setClass(
    "pubchem_structure",
    contains = "chart",
    slots = c(
        query_column = "entity",
        search_by = "enum",
        row_index = "entity",
        record_type = "enum",
        image_size = "entity"
    ),
    prototype = list(
        name = "PubChem molecular structure",
        description = paste0(
            "Query the PubChem api and retrieve a display an image of the ",
            "matching molecular structure."
        ),
        type = "image",
        .params = c(
            "query_column", "search_by", "row_index", "record_type",
            "image_size"
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
        libraries = "cowplot",
        search_by = enum(
            name = "Search by term",
            description = paste0(
                "The PubChem domain to search for matches to the ",
                "annotation_column"
            ),
            allowed = c("cid", "name", "smiles", "inchikey"),
            value = "cid"
        ),
        row_index = entity(
            name = "Row index",
            description = paste0(
                "The row index of the `annotation_source` to request an image ",
                "of the molecular structure of."
            ),
            type = c("integer", "numeric"),
            value = 1
        ),
        record_type = enum(
            name = "Record type",
            description = paste0(
                "The record type to return from the PubChem query. Can be one ",
                'of "2d" or "3d"'
            ),
            type = "character",
            allowed = c("2d", "3d"),
            value = "2d",
            max_length = 1
        ),
        image_size = entity(
            name = "Image size",
            description = paste0(
                "The size of the image to return from the PubChem query. ",
                'Can be one of "large" or "small". For `record_type = "2d"` ',
                "an arbitrary image size can be specified e.g. `123x123`."
            ),
            type = "character",
            value = "large",
            max_length = 1
        )
    )
)

#' @export
setMethod(
    f = "chart_plot",
    signature = c("pubchem_structure", "annotation_source"),
    definition = function(obj, dobj) {
        # get row
        dobj$data <- dobj$data[obj$row_index, ]
        
        # use api
        P <- pubchem_structure_lookup(
            query_column = obj$query_column,
            search_by = obj$search_by,
            record_type = obj$record_type,
            image_size = obj$image_size,
            suffix = "_.pubchem_img"
        )
        
        P <- model_apply(P, dobj)
        img <- P$updated$data[["png_.pubchem_img"]][[1]]
        img <- img / img[1, 1, 1]
        g <- cowplot::ggdraw() + cowplot::draw_image(img, scale = 0.95)
        return(g)
    }
)
