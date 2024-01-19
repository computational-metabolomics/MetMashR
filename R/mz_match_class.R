#' @eval get_description('mz_match')
#' @export
#' @include annotation_source_class.R
mz_match = function(
    variable_meta,
    mz_column,
    ppm_window,
    id_column,
    ...) {
    
    # make rt_window length 2
    if (length(ppm_window)==1) {
        ppm_window=c('variable_meta'=ppm_window,'annotations'=ppm_window)
    }
    
    # check ppm window is named if length == 2 in case user-provided
    if (!all(names(ppm_window) %in% c('variable_meta','annotations')) | is.null(names(ppm_window))) {
        stop('If providing two ppm windows then the vector must be named e.g. c("variable_meta" = 5, "annotations"= 2)')
    }
    
    out=struct::new_struct('mz_match',
        variable_meta=variable_meta,
        mz_column=mz_column,
        ppm_window=ppm_window,
        id_column=id_column,
        ...)
    return(out)
}



.mz_match<-setClass(
    "mz_match",
    contains = c('model'),
    slots=c(
        updated='entity',
        variable_meta='entity',
        mz_column='entity',
        ppm_window='entity',
        id_column='entity',
        .vm_lim='data.frame',
        .an_lim='data.frame'
    ),
    
    prototype=list(
        name = 'mz matching',
        description = paste0('Annotations will be matched to the measured data variable meta ',
            'data.frame by determining which annotations ppm window overlaps with the ppm ',
            'window from the measured mz.'),
        type = 'univariate',
        predicted = 'updated',
        .params=c('variable_meta','mz_column','ppm_window','id_column'),
        .outputs=c('updated'),
        updated=entity(
            name='Updated annotations',
            description = 'The input annotation source with the newly generated column.',
            type='annotation_source'
        ),
        variable_meta=entity(
            name='Variable meta data',
            description = 'A data.frame of variable IDs and their corresponding mz values.',
            type='data.frame'
        ),
        mz_column=entity(
            name='mz column name',
            description = 'column name of the mz values in variable_meta.',
            type='character'
        ),
        ppm_window=entity(
            name='ppm window',
            description = paste0('ppm window to use for matching. If a single value ',
                'is provided then the same ppm is used for both variable meta and ',
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
    signature=c("mz_match","annotation_source"),
    definition=function(M,D) {
        
        VM = M$variable_meta

        # ensure numeric
        VM[[M$mz_column]]=as.numeric(VM[[M$mz_column]])
        
        # use rownames for id if requested
        if (M$id_column=='rownames') {
            VM$.id=rownames(VM)
        } else {
            VM$.id=VM[[M$id_column]]
        }
        
        AN = D$data

        # ensure numeric
        AN[[D$mz_column]]=as.numeric(AN[[D$mz_column]])
        
        # calculate ppm window for variable_meta
        VM$.mz_min = M$variable_meta[[M$mz_column]] * (1-M$ppm_window[["variable_meta"]]*1e-6)
        VM$.mz_max = M$variable_meta[[M$mz_column]] * (1+M$ppm_window[["variable_meta"]]*1e-6)
        
        # calculate ppm window for annotations
        AN$.mz_min = AN[[D$mz_column]] * (1-M$ppm_window[["annotations"]]*1e-6)
        AN$.mz_max = AN[[D$mz_column]] * (1+M$ppm_window[["annotations"]]*1e-6)
        
        M@.vm_lim=data.frame(
            VM_mz_min=VM$.mz_min,
            VM_mz_max=VM$.mz_max
        )
        M@.an_lim=data.frame(
            AN_mz_min=AN$.mz_min,
            AN_mz_max=AN$.mz_max
        )
        
        # for each annotation, get variable ids whose ppw window overlaps with 
        # the annotation ppm window
        OUT=list()
        for (k in 1:nrow(AN)) {
            x=AN[k,,drop=FALSE]
            
            # true if overlap
            w=which(
                VM$.mz_min <= x$.mz_max & x$.mz_min <= VM$.mz_max)
            found=VM[w,]

            # if we found any
            if (length(w)>0) {
                # calculate mz diff
                found$mz_diff = (found[[M$mz_column]]-x[[D$mz_column]])
                # ppm difference
                found$ppm_diff = 1e6 * (found$mz_diff/x[[D$mz_column]])
                found$ppm_diff2 = 1e6 * (-found$mz_diff/found[[M$mz_column]])

                # create duplicate annotations for each id
                record_list = rep(list(x),length(w))
                record_list = do.call(rbind,record_list)
                record_list$mz_match_id = found$.id
                record_list$mz_match_diff = found$mz_diff
                record_list$ppm_match_diff_an = found$ppm_diff
                record_list$ppm_match_diff_vm = found$ppm_diff2
                record_list$mz_match = found[[M$mz_column]]
                
            } else {
                #return record with NA in id column
                record_list=x
                record_list$mz_match_id=NA
                record_list$mz_match_diff=NA
                record_list$ppm_match_diff_an=NA
                record_list$ppm_match_diff_vm=NA
                record_list$mz_match = NA
            }
            OUT[[k]]=record_list
        }
        OUT = do.call(rbind,OUT)
        
        # remove extra columns
        w=which(colnames(OUT) %in% c('.id','.mz_min','.mz_max'))
        OUT=OUT[,-w]
        
        D$data=OUT
        
        M$updated=D
        
        return(M)
    }
)





