#' @eval get_description('filter_range')
#' @export
#' @include annotation_source_class.R
filter_range = function(
    column_name,
    upper_limit=Inf,
    lower_limit=-Inf,
    equal_to=TRUE,
    ...) {
    
    if (!is(upper_limit,'function') & !is(lower_limit,'function')) {
        check = upper_limit>=lower_limit
        if (!check) {
            stop("upper_limit must be greater than the lower_limit")
        }
    }
    
    out=struct::new_struct('filter_range',
        column_name=column_name,
        upper_limit=upper_limit,
        lower_limit=lower_limit,
        equal_to=equal_to,
        ...)
    return(out)
}



.filter_range<-setClass(
    "filter_range",
    contains = c('model'),
    slots=c(
        column_name='entity',
        upper_limit='entity',
        lower_limit='entity',
        equal_to='entity',
        filtered='entity',
        flags='entity'
    ),
    
    prototype=list(
        name = 'Filter by range',
        description = paste0('Removes annotations where the names column is  greater than an upper limit or less than a lower limit.'),
        type = 'univariate',
        predicted = 'filtered',
        .params=c('column_name','upper_limit','lower_limit','equal_to'),
        .outputs=c('filtered','flags'),
        column_name = entity(
            name = 'Column name',
            description ='The column name to filter.',
            type=c('character'),
            value='V1',
            max_length=1
        ),
        upper_limit = entity(
            name = 'Upper limit',
            description ='The upper limit used for filtering. Can be a value, or a function that computes a value (e.g. mean).',
            type=c('numeric','integer','function'),
            value=Inf,
            max_length=1
        ),
        lower_limit = entity(
            name = 'Lower limit',
            description ='The lower limit used for filtering. Can be a value, or a function that computes a value (e.g. mean).',
            type=c('numeric','integer','function'),
            value=-Inf,
            max_length=1
        ),
        equal_to = entity(
            name = 'Equal to limits',
            description = c(
                'TRUE' = 'greater/less than or equal to the limits are excluded.',
                'FALSE' = 'greater/less than the limits are excluded.'
            ),
            value=TRUE,
            type='logical',
            max_length = 1
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
    signature=c("filter_range","annotation_source"),
    definition=function(M,D) {
        
        X = D$data
        
        if (nrow(X)==0) {
            # nothing to filter, so return
            M$filtered=D
            return(M)
        }
        
        # convert to numeric
        X[[M$column_name]] = as.numeric(X[[M$column_name]])
        
        if (is.function(M$upper_limit)){
            upper_limit = M$upper_limit(X[[M$column_name]])
        } else {
            upper_limit = M$upper_limit
        }
        
        if (is.function(M$lower_limit)){
            lower_limit = M$lower_limit(X[[M$column_name]])
        } else {
            lower_limit = M$lower_limit
        }
        
        check = upper_limit>=lower_limit
        if (!check) {
            stop("upper_limit must be greater than the lower_limit")
        }
        
        if (M$equal_to) {
            w = which(X[[M$column_name]] <= lower_limit | X[[M$column_name]] >= upper_limit)
        } else {
            w = which(X[[M$column_name]] < lower_limit | X[[M$column_name]] > upper_limit)
        }
        
        OUT=X[[M$column_name]]>Inf
        
        OUT[w]=TRUE
        flags = data.frame(flags=OUT,value=X[[M$column_name]],lower_limit=lower_limit,upper_limit=upper_limit)
        colnames(flags)[2]=M$column_name
        
        rownames(flags)=rownames(X)

        M$flags=flags
        
        X = X[!M$flags[,1],]

        D$data=X
        
        M$filtered=D
        
        return(M)
    }
)




