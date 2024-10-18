#' @eval get_description('BiocFileCache_database')
#' @export
#' @include annotation_database_class.R BiocFileCache_database_helpers.R
#' @family database
BiocFileCache_database <- function(
        source,
        bfc_path = NULL,
        resource_name,
        bfc_fun = .cache_as_is,
        import_fun = read.csv,
        offline = FALSE,
        ...) {
    # new object
    out <- struct::new_struct(
        "BiocFileCache_database",
        source = source,
        bfc_path = bfc_path,
        resource_name = resource_name,
        bfc_fun = bfc_fun,
        import_fun = import_fun,
        offline = offline,
        ...
    )
    return(out)
}

.BiocFileCache_database <- setClass(
    "BiocFileCache_database",
    contains = "annotation_database",
    slots = c(
        bfc_path = "entity",
        resource_name = "entity",
        bfc_fun = "entity",
        import_fun = "entity",
        offline = "entity"
    ),
    prototype = list(
        name = "Cached database",
        description = "A cached resource using BiocFileCache.",
        type = "BiocFileCache_database",
        .writable = FALSE,
        .params = c(
            "bfc_path", "resource_name", "bfc_fun", "import_fun",
            "offline"
        ),
        libraries = "BiocFileCache",
        bfc_path = entity(
            name = "BiocFileCache options",
            description = paste0(
                "`BiocFileCache` is used to cache the database ",
                "locally and prevent unnecessary downloads. If a path is ",
                "provided then `BiocFileCache` will use this location. ",
                "If NULL it will use the default location ",
                "(see [BiocFileCache::BiocFileCache()] for details)."
            ),
            type = c("character", "NULL"),
            value = NULL,
            max_length = 1
        ),
        resource_name = entity(
            name = "BiocFileCache resource name",
            description = paste0(
                "The name given to this resource in the cache. ",
                "(see [BiocFileCache::BiocFileCache()] for details)"
            ),
            type = c("character"),
            value = "bfc",
            max_length = 1
        ),
        bfc_fun = entity(
            name = "BiocFileCache function",
            description = paste0(
                "A function to process the object before storing it in the ",
                "cache, e.g. to store an unzipped file in the cache instead ",
                "of the zipped version. This would prevent needing to unzip ",
                "the resource each time it is retrieved from the cache, but ",
                "would mean using more space on disk. The default function ",
                "does nothing to the resource. ",
                "See [BiocFileCache::bfcdownload()] for details."
            ),
            type = c("function"),
            value = .cache_as_is,
            max_length = 1
        ),
        import_fun = entity(
            name = "Import function",
            description = paste0(
                "A function to process the object after retrieving it from ",
                "the cache e.g. it might need to be unzipped before importing",
                "as a data.frame. This function should take the path to the ",
                "cached object as the first input and return a data.frame."
            ),
            type = c("function"),
            value = function(path, params) {
                return(data.frame())
            },
            max_length = 1
        ),
        offline = entity(
            name = "Offline",
            description = paste0(
                "If `offline  = FALSE` then checks to determine if the ",
                "resource ",
                "has expired will be skipped, and retrieved directly from ",
                "the cache."
            ),
            type = "logical",
            value = FALSE
        )
    )
)

#' @export
#' @rdname read_database
setMethod(
    f = "read_database",
    signature = c("BiocFileCache_database"),
    definition = function(obj) {
        if (is.null(obj$bfc_path)) {
            obj$bfc_path <- BiocFileCache::getBFCOption("CACHE")
        }
        
        # get path
        path <- .get_cached_path(obj)
        
        # read
        df <- obj$import_fun(path)
        
        # return
        return(df)
    }
)

# internal function to check for resource in cache, retrieve it if not present,
# and return path to it in cache
.get_cached_path <- function(obj) {
    # select cache
    bfc <- BiocFileCache::BiocFileCache(obj$bfc_path)
    # name of resource
    rname <- paste0("MetMashR_", obj$resource_name)
    # query cache
    rid <- BiocFileCache::bfcquery(
        x = bfc,
        query = obj$source,
        field = "fpath", exact = TRUE
    )$rid
    
    # if not present, then add it
    if (!length(rid)) {
        rid <- names(
            BiocFileCache::bfcadd(
                x = bfc,
                fpath = obj$source,
                rname = rname,
                download = FALSE
            )
        )
    }
    
    if (rid %in% BiocFileCache::bfcquery(
        bfc,
        field = "rtype", 
        query = "web")$rid) {
        # TRUE if newly added or stale
        update <- BiocFileCache::bfcneedsupdate(bfc, rid)
        if (is.na(update)) { # FALSE if NA
            update <- FALSE
        }
    } else {
        update <- FALSE # cant update if not web resource
    }
    
    
    # download & unzip
    if (update & !obj$offline) {
        BiocFileCache::bfcdownload(
            x = bfc,
            rid = rid,
            ask = FALSE,
            FUN = obj$bfc_fun,
            verbose = FALSE
        )
    }
    
    # get path
    path <- BiocFileCache::bfcrpath(bfc, rids = rid)
    
    return(path)
}
