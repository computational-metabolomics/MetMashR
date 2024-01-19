# unzip MTox and import relevant file
.MTox700plus_import_fun = function(path){
    # get folders
    fldrs = unzip(path,list=TRUE)
    
    # connection to file in zip
    conn = unz(path,paste0(fldrs$Name[1],'MTox700plus.csv'))
    
    # read
    df = read.csv(conn)
    
    return(df)
}

#' Unzip file before caching with BiocFileCache_database
#' 
#' This helper function is for use with [`BiocFileCache_database()`] objects. 
#' Using it as the 
#' `bfc_fun` input for this object will unzip a downloaded resource into a 
#' temporary folder before storing it in the cache.
#' @param from incoming path
#' @param to the outgoing path
#' @return TRUE if successful
#' @examples
#' M = BiocFileCache_database(
#'     path = tempfile(),
#'     resource_name = 'example',
#'     bfc_fun=.unzip)
#' 
#' @export
.unzip = function(from,to){
    # temp folder
    fldr = tempdir()
    # unzip
    fn = unzip(from,overwrite = TRUE,exdir = fldr)
    # rename
    file.copy(from = fn,to = to, overwrite = TRUE)
    # return
    return(TRUE)
}

#' Cache file with no changes using BiocFileCache
#' 
#' This helper function is for use with `BiocFileCache` objects. Using it will
#' copy the file directly to the cache without making any changes.
#' @param from incoming path
#' @param to the outgoing path
#' @return TRUE if successful
#' @examples
#' M = BiocFileCache_database(
#'     path = tempfile(),
#'     resource_name = 'example',
#'     bfc_fun=.cache_as_is)
#' 
#' @export
.cache_as_is = function(from,to){
    file.copy(from = from,to = to, overwrite = TRUE)
    return(TRUE)
}
