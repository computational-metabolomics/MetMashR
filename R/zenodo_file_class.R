#' @eval get_description('zenodo_file')
#' @export
#' @include annotation_database_class.R BiocFileCache_database_class.R zzz.R
zenodo_file <- function(record_id,
    file_name,
    bfc_path = NULL,
    resource_name = paste("zenodo", record_id, file_name, sep = "_"),
    ...) {
    # new object
    out <- struct::new_struct(
        "zenodo_file",
        record_id = record_id,
        file_name = file_name,
        bfc_path = bfc_path,
        resource_name = resource_name,
        ...
    )
    return(out)
}

.zenodo_file <- setClass(
    "zenodo_file",
    contains = "BiocFileCache_database",
    slots = c(
        record_id = "entity",
        file_name = "entity"
    ),
    prototype = list(
        name = "Zenodo file",
        description = paste0(
            "Retrieves a file from a Zenodo record and caches it locally ",
            "using `BiocFileCache`."
        ),
        type = "zenodo_source",
        .params = c("record_id", "file_name"),
        libraries = "BiocFileCache",
        record_id = entity(
            name = "Zenodo record ID",
            description = paste0(
                "The ID of the Zenodo record containing the file, e.g. ",
                "8226097 for doi:10.5281/zenodo.8226097. Each version of a ",
                "Zenodo record has its own ID."
            ),
            type = c("character", "numeric"),
            max_length = 1
        ),
        file_name = entity(
            name = "Zenodo file name",
            description = paste0(
                "The name of the file to download from the Zenodo record."
            ),
            type = "character",
            max_length = 1
        ),
        bfc_fun = .set_entity_value(
            obj = "BiocFileCache_database",
            param_id = "bfc_fun",
            value = cache_as_is
        )
    )
)


#' @export
#' @rdname read_database
setMethod(
    f = "read_database",
    signature = c("zenodo_file"), definition = function(obj) {
        # the cached copy keeps the file name (and extension) from the url
        obj$source <- paste0(
            "https://zenodo.org/records/", obj$record_id, "/files/",
            utils::URLencode(obj$file_name, reserved = TRUE)
        )
        df <- callNextMethod(obj)

        # return
        return(df)
    }
)
