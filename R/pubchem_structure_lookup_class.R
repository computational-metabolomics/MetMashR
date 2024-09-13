#' @include annotation_source_class.R rest_api_class.R
pubchem_structure_lookup <- function(
        query_column,
        search_by,
        suffix = "_pubchem",
        record_type = "2d",
        image_size = "small",
        ...) {
    if (record_type == "3d") {
        check <- image_size %in% c("small", "large")
        if (!check) {
            stop("Cannot (yet) generate arbitrarily-sized 3D images")
        }
    }
    
    out <- struct::new_struct(
        "pubchem_structure_lookup",
        query_column = query_column,
        search_by = search_by,
        suffix = suffix,
        record_type = record_type,
        image_size = image_size,
        ...
    )
    
    return(out)
}

.pubchem_structure_lookup <- setClass(
    "pubchem_structure_lookup",
    contains = c("pubchem_compound_lookup"),
    slots = c(
        record_type = "enum",
        image_size = "entity"
    ),
    prototype = list(
        name = "Compound ID lookup via pubchem",
        description = paste0(
            "Uses the PubChem API to search for images of molecular structures"
        ),
        type = "rest_api",
        predicted = "updated",
        .params = c("record_type", "image_size"),
        url_template = entity(
            name = "URL template",
            description = paste0(
                "A template describing how the URL should be ",
                "constructed from the base URL and input parameters. e.g. ",
                "<base_url>/<context>/<input_item>/<search_term>/json.",
                "The url will be constructed by replacing the values ",
                "enclosed in <> with the value from corresponding input ",
                "parameter of the rest_api object. A term preceeded by . will ",
                "assume substitute values from a column name from the  ",
                "annotation table."
            ),
            value = paste0(
                "<base_url>/<search_by>/<query_column>/PNG?",
                "record_type=<record_type>&image_size=<image_size>"
            ),
            max_length = 1
        ),
        status_codes = entity(
            name = "Status codes",
            description = paste0(
                "Named list of status codes and function indicating how to ",
                "respond. Should minimally contain a function to parse a ",
                "response for status code 200."
            ),
            type = "list",
            value = list(
                "200" = # get content and put into data.frame
                    function(response, ...) {
                        return(data.frame(png = I(list(content(response)))))
                    },
                "404" = function(...) {
                    return(NULL)
                }
            ),
            max_length = Inf
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
                'Can be one of "large" or "small". For `record_type = "2d" ',
                "an arbitrary image size can be specified e.g. 123x123."
            ),
            type = "character",
            value = "large",
            max_length = 1
        )
    )
)
