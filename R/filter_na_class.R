#' @eval get_description('filter_na')
#' @export
#' @include annotation_source_class.R
filter_na = function(
        column_name,
        ...) {
    out=struct::new_struct('filter_na',
        column_name=column_name,
        ...)
    return(out)
}

.filter_na<-setClass(
    "filter_na",
    contains = c('model'),
    slots=c(
        column_name='entity',
        filtered='entity',
        flags='entity'
    ),
    
    prototype=list(
        name = 'Filter by range',
        description = paste0('Removes annotations where the names column is  greater than an upper limit or less than a lower limit.'),
        type = 'univariate',
        predicted = 'filtered',
        .params=c('column_name'),
        .outputs=c('filtered','flags'),
        column_name = entity(
            name = 'Column name',
            description ='The column name to filter.',
            type=c('character'),
            value='V1',
            max_length=1
        ),
        filtered=entity(
            name = 'Filtered annotations',
            description = 'annotation_source after filtering.',
            type='annotation_source',
            max_length = Inf
        ),
        flags=entity(
            name = 'Flags',
            description = 'A list of flags indicating which annotations were removed.',
            value=data.frame(),
            type='data.frame',
            max_length = Inf
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("filter_na","annotation_source"),
    definition=function(M,D) {
        
        X = D$data
        
        if (nrow(X)==0) {
            # nothing to filter, so return
            M$filtered=D
            return(M)
        }
        
       
        flags = data.frame(na_flag=is.na(X[[M$column_name]]),value=X[[M$column_name]])
        colnames(flags)[2]=M$column_name
        
        rownames(flags)=rownames(X)
        
        M$flags=flags
        
        X = X[!M$flags[,1],]
        
        D$data=X
        
        M$filtered=D
        
        return(M)
    }
)

