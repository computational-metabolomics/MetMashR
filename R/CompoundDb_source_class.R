#' @eval get_description('CompoundDb_source')
#' @export
#' @include annotation_source_class.R annotation_source_class.R
CompoundDb_source = function(
        source,
        tag='cdb',...) {
    out=struct::new_struct(
        'CompoundDb_source',
        source=source,
        tag=tag,
        ...)
    return(out)
}


.CompoundDb_source<-setClass(
    "CompoundDb_source",
    contains = c('annotation_source'),
    prototype=list(
        name = 'Import CompDB source',
        description = paste0(
            'Imports the compounds table of a CompDB source as an ',
            '`annotation_source`.'),
        libraries = 'CompoundDb'
    )
    
)

#' @export
setMethod(f = "model_apply",
          signature = c("CompoundDb_source","annotation_source"),
          definition = function(M,D) {

              # check db exists
              stopifnot(file.exists(M$source))
              
              # connect
              db = CompoundDb::CompDb(M$source)
              
              # get compounds table
              df = CompoundDb::compounds(
                  df,
                  return.type = "data.frame",
                  columns = compoundVariables(df,includeId=TRUE)
              )
              
              # add tag, id col
              D$tag=M$tag
              D$id_column='compound_id'
              
              # assign to annotation table
              D$data=df
              
              # add to object
              M$imported=D
              
              # done
              return(M)
          }
)



