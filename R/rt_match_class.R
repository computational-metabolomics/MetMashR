#' @eval get_description('rt_match')
#' @export
#' @include annotation_table_class.R
rt_match = function(
    variable_meta,
    rt_column,
    rt_window,
    id_column,
    ...) {
    
    # make rt_window length 2
    if (length(rt_window)==1) {
        rt_window=c('variable_meta'=rt_window,'annotations'=rt_window)
    }
    
    # check rt window is named if length == 2 in case user-provided
    if (!all(names(rt_window) %in% c('variable_meta','annotations')) 
        | is.null(names(rt_window))) {
        stop('If providing two retention time windows then the vector must be ',
             'named e.g. c("variable_meta" = 5, "annotations"= 2)')
    }

    out=struct::new_struct('rt_match',
        variable_meta=variable_meta,
        rt_column=rt_column,
        rt_window=rt_window,
        id_column=id_column,
        ...)
    return(out)
}



.rt_match<-setClass(
    "rt_match",
    contains = c('model'),
    slots=c(
        updated='entity',
        variable_meta='entity',
        rt_column='entity',
        rt_window='entity',
        id_column='entity'
    ),
    
    prototype=list(
        name = 'rt matching',
        description = paste0('Annotations will be matched to the measured variable meta ',
            'data.frame by determining which annotations rt window overlaps with the rt ',
            'window from the measured rt.'),
        type = 'univariate',
        predicted = 'updated',
        .params=c('variable_meta','rt_column','rt_window','id_column'),
        .outputs=c('updated'),
        updated=entity(
            name='Updated annotations',
            description = 'The input annotation source with the newly generated column.',
            type='annotation_table'
        ),
        variable_meta=entity(
            name='Variable meta data',
            description = 'A data.frame of variable IDs and their corresponding rt values.',
            type='data.frame'
        ),
        rt_column=entity(
            name='rt column name',
            description = 'column name of the rt values in variable_meta.',
            type='character'
        ),
        rt_window=entity(
            name='rt window',
            description = paste0('rt window to use for matching. If a single value ',
                'is provided then the same rt is used for both variable meta and ',
                'the annotations. A named vector can also be provided ',
                'e.g. c("variable_meta"=5,"annotations"=2) to use different windows ',
                'for each data table.'),
            type=c('numeric','integer'),
            max_length=2
        ),
        id_column=entity(
            name='id column name',
            description = 'column name of the variable ids in variable_meta. id_column="rownames" will use the rownames as ids.',
            type='character'
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("rt_match","annotation_table"),
    definition=function(M,D) {
        
        VM = M$variable_meta
        
        # ensure numeric
        VM[[M$rt_column]]=as.numeric(VM[[M$rt_column]])
        
        # use rownames for id if requested
        if (M$id_column=='rownames') {
            VM$.id=rownames(VM)
        } else {
            VM$.id=VM[[M$id_column]]
        }
        
        AN = D$data
        
        # ensure numeric
        AN[[D$rt_column]]=as.numeric(AN[[D$rt_column]])
        
        
        # calculate rt window for variable_meta
        VM$.rt_min = M$variable_meta[[M$rt_column]] - M$rt_window[['variable_meta']]
        VM$.rt_max = M$variable_meta[[M$rt_column]] + M$rt_window[['variable_meta']]
        
        # calculate rt window for annotations
        AN$.rt_min = AN[[D$rt_column]] - M$rt_window[['annotations']]
        AN$.rt_max = AN[[D$rt_column]] + M$rt_window[['annotations']]
        
        # for each annotation, get variable ids whose rt window overlaps with 
        # the annotation rt window
        OUT=list()
        for (k in 1:nrow(AN)) {
            
            x = AN[k,,drop=FALSE]
            
            # true if overlap
            w = which(
                    VM$.rt_min <= x$.rt_max & x$.rt_min <= VM$.rt_max)
            found=VM[w,]
            
            # if we found any
            if (length(w)>0) {
                # calculate rt diff
                found$rt_diff = (found[[M$rt_column]]-x[[D$rt_column]])

                # create duplicate annotations for each id
                record_list = rep(list(x),length(w))
                record_list = do.call(rbind,record_list)
                record_list$rt_match_id = found$.id
                record_list$rt_match_diff = found$rt_diff
                record_list$rt_match = found[[M$rt_column]]

            } else {
                # return record with NA in id column
                record_list=x
                record_list$rt_match_id=NA
                record_list$rt_match_diff=NA
                record_list$rt_match = NA
            }
            OUT[[k]]=record_list
        }
        OUT = do.call(rbind,OUT)
        
        # remove extra columns
        w=which(colnames(OUT) %in% c('.id','.rt_min','.rt_max'))
        OUT=OUT[,-w]
        
        D$data=OUT
        
        M$updated=D
        
        return(M)
    }
)




