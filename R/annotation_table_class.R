#' @eval get_description("annotation_table")
#' @include annotation_source_class.R
#' @family {annotation tables}
#' @family {annotation sources}
#' @export
annotation_table = function(
        data = data.frame(id = character(0)),
        tag = '',
        id_column = 'id',
        ...) {
    
    # if null, create row id column
    if (is.null(id_column)) {
        id_column = '.MetMashR_id'
        data[[id_column]] = 1:nrow(data)
    }
    
    # new object
    out = new_struct(
        'annotation_table',
        data = data,
        id_column = id_column,
        tag = tag,
        ...)
    
    return(out)
}

.annotation_table<-setClass(
    "annotation_table",
    contains = c('annotation_source'),
    slots = c(
        id_column = 'entity'),
    prototype = list(
        name = 'An annotation table',
        description = paste0(
            'An `annotation_table` is an [`annotation_source()`] where the ',
            'imported data.frame contains measured experimental data. An 
            `id_column` of values is required to uniquely indentify each ',
            'record (row) in the ',
            'table (NB these are NOT molecule identifiers, which may be ',
            'be present in multiple records).'),
        id_column = entity(
            name = 'Annotation row identifiers',
            description=paste0(
                'The column name of the annotation data.frame containing row ',
                'identifers. If NULL This will be generated automatically.'),
            type='character',
            max_length=1,
            value = 'id'
        ),
        .params=c('id_column')
    )
)

setValidity('annotation_table',
            function(object){
                msg = check_for_columns(
                    object,
                    'id_column' = object$id_column,
                    msg = TRUE)      
                return(msg)
            })

#' @eval get_description("annotation_table")
#' @export lcms_table
#' @family {annotation_tables}
lcms_table = function(
        data = data.frame(
            id = numeric(0),
            mz = numeric(0),
            rt = numeric(0)
        ),
        tag='',
        id_column = 'id',
        mz_column = 'mz',
        rt_column = 'rt',
        ...) {
    
    
    # new object
    out = new_struct('lcms_table',
                     data = data,
                     id_column = id_column,
                     tag = tag,
                     mz_column = mz_column,
                     rt_column = rt_column,
                     ...)
    return(out)
}

.lcms_table<-setClass(
    "lcms_table",
    contains = c('annotation_table'),
    slots = c(
        mz_column = 'entity',
        rt_column = 'entity'
    ),
    prototype = list(
        name = 'LCMS table',
        description = paste0(
            'An LCMS table extends [`annotation_table()`] to represent ',
            'annotation data for an LCMS experiment. Columns representing ',
            'm/z and retention time are required for an `lcms_table`.'),
        mz_column = entity(
            name = 'm/z column name',
            description=paste0(
                'The column name of the annotation data.frame containing m/z ',
                'values.'),
            type='character',
            max_length = 1,
            value = 'mz'
        ),
        rt_column = entity(
            name = 'Retention time column name',
            description = paste0(
                'The column name of the annotation data.frame containing ',
                'retention time values.'),
            type = 'character',
            max_length = 1,
            value = 'rt'
        ),
        .params = c('mz_column','rt_column')
    )
)

setValidity('lcms_table',
            function(object){
                
                msg = check_for_columns(
                    object,
                    'id_column' = object$id_column,
                    'mz_column' = object$mz_column,
                    'rt_column' = object$rt_column,
                    msg = TRUE)
                
                return(msg)
            }
)


