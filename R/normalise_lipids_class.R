#' @eval get_description('normalise_lipids')
#' @export
#' @include annotation_source_class.R
normalise_lipids = function(
        column_name,
        grammar,
    ...) {
    out=struct::new_struct('normalise_lipids',
        column_name=column_name,
        grammar = grammar,
        ...)
    return(out)
}

.normalise_lipids<-setClass(
    "normalise_lipids",
    contains = c('model'),
    slots=c(
        column_name='entity',
        updated='entity',
        grammar = 'enum'
    ),
    
    prototype=list(
        name = 'Normalise Lipids nomenclature',
        description = paste0('Normalises differently formated lipid names to ',
            'a consistent format.'),
        type = 'univariate',
        predicted = 'updated',
        libraries = 'rgoslin',
        .params=c('column_name','grammar'),
        .outputs=c('updated'),
        column_name = entity(
            name = 'Lipid column name',
            description ='The name of the column containing Lipids names to normalise.',
            type=c('character'),
            value='V1',
            max_length=1
        ),
        updated=entity(
            name = 'Updated annotations',
            description = 'annotation_source after normalising lipid names',
            type='annotation_source',
            max_length = Inf
        ),
        grammar = enum(
            name = 'Grammar',
            description = paste0('The grammar to use for normalising lipid ',
                'names. Allowed values are: Shorthand2020, Goslin, FattyAcids, ',
                'LipidMaps, SwissLipids, HMDB or .all'),
            type = 'character',
            allowed = c(
                "Shorthand2020",
                "Goslin",
                "FattyAcids",
                "LipidMaps",
                "SwissLipids",
                "HMDB",
                '.all'
            ),
            value = '.all'
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("normalise_lipids","annotation_source"),
    definition=function(M,D) {
        
        X = D$data
        
        if (M$grammar == '.all') {
            grammar = NULL
        } else {
            grammar = M$grammar
        }
        
        df = rgoslin::parseLipidNames(
                lipidNames = X[[M$column_name]],
                grammar = grammar
        )
        
        X = cbind(X,df)
        
        D$data = X
        M$updated = D
        
        return(M)
    }
)

