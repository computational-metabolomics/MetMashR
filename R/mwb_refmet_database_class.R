#' @eval get_description('mwb_refmet_database')
#' @export
#' @include annotation_database_class.R
mwb_refmet_database <- function(
        bfc = NULL,
        ...) {
    # replace NULL with default
    if (is.null(bfc)) {
        bfc <- BiocFileCache::getBFCOption("CACHE")
    }
    
    # new object
    out <- struct::new_struct(
        "mwb_refmet_database",
        bfc = bfc,
        ...
    )
    return(out)
}

.mwb_refmet_database <- setClass(
    "mwb_refmet_database",
    contains = "annotation_database",
    slots = c(version = "entity", bfc = "entity"),
    prototype = list(
        name = "mwb_refmet_database",
        description = paste0(
            "Imports the Metabolomics Workbench refmet database."
        ),
        type = "database",
        .params = c("bfc"),
        libraries = c("BiocFileCache", "httr", "plyr"),
        bfc = entity(
            name = "BiocFileCache options",
            description = paste0(
                "`BiocFileCache` is used to cache database ",
                "locally and prevent unnecessary downloads. If a path is ",
                "provided then `BiocFileCache` will use this location. ",
                "If NULL it will use the default location ",
                "(see [BiocFileCache::BiocFileCache] for details)."
            ),
            type = c("character"),
            value = NULL
        )
    )
)


#' @export
#' @rdname read_database
setMethod(
    f = "read_database",
    signature = c("mwb_refmet_database"), definition = function(obj) {
        # url
        u <- "https://www.metabolomicsworkbench.org/rest/refmet/all"
        
        # cache
        bfc <- BiocFileCache::BiocFileCache(obj$bfc)
        path <- BiocFileCache::bfcrpath(bfc, u)
        
        # read
        df <- jsonlite::fromJSON(path)
        
        # convert to df
        df <- lapply(df, as.data.frame)
        df <- plyr::rbind.fill(df)
        
        # return
        return(df)
    }
)
