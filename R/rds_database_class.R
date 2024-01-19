#' @eval get_description('rds_database')
#' @export
#' @include annotation_database_class.R
#' @family {annotation databases}
rds_database = function(
        source = character(0),
        ...) {
    
    # new object
    out = struct::new_struct(
        'rds_database',
        source = source,
        ...
    )
    return(out)
}

.rds_database<-setClass(
    "rds_database",
    contains = 'annotation_database',
    prototype = list(
        name = 'rds database', 
        description='A data.frame stored as an RDS file.', 
        type='rds_database',
        .writable = TRUE
    )
)

#' @export
setMethod(f="read_database",
          signature=c("rds_database"),definition = function(obj) {
              # read the file
              IN = readRDS(obj$source)
              
              # return
              return(IN)
          }
)

#' @export
setMethod(f="write_database",
          signature=c("rds_database"),definition = function(obj,df) {
              
              check = is_writable(obj)
              if (!check){
                  stop('This ',class(obj),' is not writable. Check file ',
                       'permissions.')
              }
              
              # write df to file
              saveRDS(object = df,file = obj$source)
              
              # return
              return(invisible(TRUE))
          }
)

#' @export
setMethod(f = "is_writable",
          signature = c("rds_database"),
          definition = function(obj) {
              return(obj@.writable & file.access(obj$source,mode=2))
          }
)


