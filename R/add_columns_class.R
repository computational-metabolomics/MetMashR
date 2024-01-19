#' @eval get_description('add_columns')
#' @export
#' @include annotation_source_class.R
#' @seealso [dplyr::left_join()]
add_columns = function(new_columns,by,...) {
    
    check=all(by %in% colnames(new_columns))
    if (!check) {
        stop('parameter "by" must be a column name of the "new_columns",
            " data.frame')
    }
    
    out=struct::new_struct(
        'add_columns',
        new_columns = new_columns,
        by = by,
        ...)
    return(out)
}


.add_columns<-setClass(
    "add_columns",
    contains = c('model'),
    
    slots = c(
        updated='entity',
        new_columns = 'entity',
        by = 'entity'),
    
    prototype=list(
        name = 'Add columns',
        description = paste0(
            'A wrapper around [`dplyr::left_join`]. Adds columns ',
            'to an annotation table by performing a left-join with an input ',
            'data.frame (annotations on the left of the join).'),
        type = 'left_join',
        predicted = 'updated',
        .params = c('new_columns','by'),
        .outputs=c('updated'),
        libraries = 'dplyr',
        
        updated = entity(
            name = 'Updated annotations',
            description=paste0(
                'The updated annotations as an `annotation_source` object'),
            type='annotation_source'
        ),
        new_columns = entity(
            name = 'New columns',
            description = paste0(
                'A data.frame to be left-joined to the ',
                'annotation table. Can also be an annotation_database.'),
            type = c('data.frame','annotation_database'),
            value = data.frame(id=NA)
        ),
        by = entity(
            name = 'By',
            description = paste0(
                'A (named) character vector of column names to join ',
                'by e.g. `c("A" = "B")` (see ',
                '[`dplyr::left_join`] ',
                'for details)'),
            type='character',
            value  = 'id'
        )
    )
)


#' @export
setMethod(f="model_apply",
          signature=c("add_columns","annotation_source"),
          definition=function(M,D) {
              
              if (is(M$new_columns,'annotation_database')) {
                  M$new_columns = read_database(M$new_columns)
              }
              
              A = D$data
              B = M$new_columns
              
              C = dplyr::left_join(A,B,by=M$by)
              
              D$data = C
              M$updated = D
              
              return(M)
          }
)




