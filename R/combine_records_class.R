#' @eval MetMasheR:::get_description('combine_records')
#' @export
#' @include annotation_source_class.R
combine_records = function(
        group_by,
    default_fcn=.collapse(separator = ' || '),
    fcns = list(),
    ...) {
    
    # check fcns are all functions
    if (length(fcns)>0) {
        check=all(unlist(lapply(fcns,function(x){
            is(x,'function') | is(x,'call')
        })))
        if (!check) {
            stop("all fcns list items must be functions or calls.")
        }
    }
    
    out=struct::new_struct('combine_records',
        default_fcn=default_fcn,
        fcns = fcns,
        group_by=group_by,
        ...)
    return(out)
}



.combine_records<-setClass(
    "combine_records",
    contains = c('model'),
    slots=c(
        updated='entity',
        fcns='entity',
        group_by='entity',
        default_fcn='entity'
    ),
    
    prototype=list(
        name = 'Combine annotation records (rows)',
        description = paste0('Combine annotation records (rows) based on a key. ',
            'All records with the same key will be combined. A number of helper ',
            'functions are provided for common approaches to merging records.'),
        type = 'univariate',
        predicted = 'updated',
        libraries='dplyr',
        citations=list(
            bibentry(
                bibtype ='Article',
                year = 2020,
                volume = 36,
                number = "22-23",
                pages = '5551-5552',
                author = as.person("Gavin Rhys Lloyd, Andris Jankevics and Ralf J M Weber"),
                title = paste0('struct: an R/Bioconductor-based framework for ',
                    'standardized metabolomics data analysis and beyond'),
                journal = "Bioinformatics"
            )
        ),
        .params=c('fcns','group_by','default_fcn'),
        .outputs=c('updated'),
        updated=entity(
            name='Updated annotations',
            description = 'The input annotation source with the newly generated column.',
            type='annotation_source'
        ),
        fcns=entity(
            name='Functions',
            description = 'A named list of functions to use for summarising named columns when combining records. Names should correspond to the columns in the annotation table.',
            type='list'
        ),
        default_fcn=entity(
            name='Default functions',
            description = 'The default function to use for summarising columns when combining records and a specific function has not been provided in fcns.',
            type=c('function'),
            value=function(x){}
        ),
        group_by=entity(
            name='Group by column',
            description = 'The column used as the key for grouping records.',
            type='character'
        )
    )
)


#' @export
setMethod(f="model_apply",
    signature=c("combine_records","annotation_source"),
    definition=function(M,D) {
        
        X=D$data
        
        # for any NA, generate a unique id so that we dont lose them during grouping
        for (k in M$group_by) {
            w=which(is.na(X[[k]]))
            if (length(w)>0) {
                X[[k]][w]=paste0('._',k,'_NA_',w)
            }
        }
        
        # if length(group_by) > 1 then combine into single column
        clean = FALSE
        orig_group=M$group_by
        if (length(M$group_by)>1) {
            str=paste(M$group_by,collapse='_x_')
            X[[str]]=do.call(paste,c(as.list(X[,M$group_by]),sep='_'))
            M$group_by = str
            clean = TRUE
        }
        
        # create list of default functions
        FCNS = rep(list(M$default_fcn),ncol(X))
        names(FCNS) = colnames(X)
        
        # add functions for specific columns
        FCNS[names(M$fcns)]=M$fcns
        
        # prep for summarise
        for (k in names(FCNS)) {
            if (k %in% colnames(X)) {
                FCNS[[k]]=expr(across(all_of(!!k),!!FCNS[[k]],.names=!!paste0('.',k)))
            } else {
                FCNS[[k]]=as.call(FCNS[k])
            }
        }
        
        # remove group by column fcn
        FCNS[M$group_by]=NULL
        
        # split into existing and new columns
        FCNSE = FCNS[names(FCNS) %in% colnames(X)]
        FCNSN = FCNS[!(names(FCNS) %in% colnames(X))]
        Y = X %>% group_by_at(M$group_by) 
        Z = do.call(reframe,c(list('.data'=Y),unname(FCNSE),FCNSN))
        
        Z = as.data.frame(Z)
        
        colnames(Z)[2:ncol(Z)]=names(FCNS)
        
        # remove extra column if created
        if (clean) {
            Z[[M$group_by]]=NULL
            M$group_by = orig_group
        }
        
        # remove generated ids for NA if created
        for (k in M$group_by) {
            w=which(grepl(pattern = "^\\.\\_",x = Z[[k]],perl = TRUE)) # match ._ at start of id
            if (length(w)>0) {
                Z[[k]][w]=NA
            }
        }
        
        D$data=Z
        M$updated=D
        
        
        return(M)
    }
)

#' Combine records helper functions
#' 
#' This page documents helper functions for use with [`combine_records()`].
#' 
#' @name combine_records_helper_functions
#' @return A function for use with [`combine_records()`]
NULL 

#' Modal value
#'
#' @param ties (logical) If TRUE then all records matching the tied groups 
#' are returned. Otherwise the first record is returned.
#' @param na.rm (logical) If TRUE then NA is ignored
#' @export
#' @describeIn combine_records_helper_functions returns the most common value, 
#' excluding NA. If `ties == TRUE` then all tied 
#' values are returned, otherwise the first value in
#' a sorted unique list is returned (equal to min if numeric).
#' If `na.rm = FALSE` then NA are included when searching for the modal value
#' and placed last if `ties = FALSE` (values are returned preferentially over NA).
#' 
.mode= function(ties=FALSE,na.rm=TRUE) {
    fcn=expr(function(x) {
        if (length(x) <= 2) return(x[1])
        if (anyNA(x) & !!na.rm) x = x[!is.na(x)]
        ux <- sort(unique(x),na.last = TRUE)
        
        if (!(!!ties)) {
            out = ux[which.max(tabulate(match(x, ux)))]
        } else {
            t = tabulate(match(x, ux))
            hi = max(t)
            w = which(t==hi)
            out=ux[w]
        }
        return(out)
    }
    )
    return(eval(fcn))
}


#' Calculate the mean value
#' @export
#' @describeIn combine_records_helper_functions calculates the mean value, 
#' excluding NA if `na.rm = TRUE`
.mean = function(){
    fcn = expr(function(x){mean(x,na.rm = TRUE)})
    return(eval(fcn))
}

#' Calculate the median value
#' @describeIn combine_records_helper_functions calculates the median value, 
#' excluding NA if `na.rm = TRUE`
#' @export
.median = function(){
    fcn = expr(function(x){stats::median(x,na.rm = TRUE)})
    return(eval(fcn))
}

#' Collapse multiple annotations
#' @describeIn combine_records_helper_functions collapses multiple matching 
#' records into a single string using the provided separator.
#' @param separator (character) a string used to separate multiple matches.
#' @param na_string (character) a string used to represent NA values.
#' @export
.collapse = function(separator,na_string='NA'){
    fcn = expr(function(x) {
        x[is.na(x)]=!!na_string
        paste0(x,collapse=!!separator)})
    return(eval(fcn))
}

#' Select annotations with a maximum
#' @describeIn combine_records_helper_functions selects a record based on 
#' the index of the maximum value in a another column.
#' @param max_col (character) the column name to search for the maximum value.
#' @param use_abs (logical) If TRUE then the sign of the values is ignored
#' @param keep_NA (logical) If TRUE then records with NA are returned as well as
#' the record with the maximum value.
#' @export
.select_max = function(max_col,use_abs=FALSE,keep_NA=FALSE){
    fcn = expr(function(x) {
        # get values
        vals=as.numeric(pick(!!max_col)[[1]])
        # if all NA, return all records
        if (all(is.na(vals))) {
            return(x)
        }
        # use abs if requested
        if (!!use_abs) {
            vals=abs(vals)
        } 
        
        w2=integer()
        if (!all(is.na(vals))) {
            # get index of max
            w  = which.max(vals)
            # find all matches to the min
            w2 = which(vals==max(vals,na.rm=TRUE)) 
        }
        
        # find NA if requested
        if (!!keep_NA) {
            w2=c(w2,which(is.na(vals)))
        }
        
        return(x[w2])
    })
    return(eval(fcn))
}

#' Select annotations with a minimum
#' @describeIn combine_records_helper_functions selects a record based on the 
#' index of the minimum in a second column.
#' @param min_col (character) the column name to search for the minimum value.
#' @param use_abs (logical) If TRUE then the sign of the values is ignored.
#' @param keep_NA (logical) If TRUE then records with NA are returned as well as
#' the record with the minimum value.
#' @export
.select_min = function(min_col,use_abs=FALSE,keep_NA=FALSE) {
    fcn = expr(function(x) {
        # get values
        vals=as.numeric(pick(!!min_col)[[1]])

        # use abs if requested
        if (!!use_abs) {
            vals=abs(vals)
        } 
        
        w2=integer()
        if (!all(is.na(vals))) {
            # get index of min
            w  = which.min(vals)
            # find all matches to the min
            w2 = which(vals==min(vals,na.rm=TRUE)) 
        }
            
        # find NA if requested
        if (!!keep_NA) {
            w2=c(w2,which(is.na(vals)))
        }

        return(x[w2])
    })
    return(eval(fcn))
}

#' Select matching annotations
#' @describeIn combine_records_helper_functions returns all records based on 
#' the indices of identical matches in a second column and collapses them 
#' useing the provided separator.
#' @param match_col (character) the name of a column to search for matches to the 
#' search column.
#' @param search_col (character) the name of a column to use as a reference for
#' locating values in the matching column.
#' @export
.select_match = function(match_col,search_col,separator,na_string='NA') {
    fcn = expr(function(x){
        x=x[which(pick(!!search_col)[[1]]==pick(!!match_col)[[1]])]
        if (!is.null(!!separator)) {
            x[is.na(x)]=!!na_string
            x=unique(x)
            paste0(x,collapse=!!separator)
        } else {
            return(x)
        }
    })
    return(eval(fcn))
}

#' Select exactly matching annotations
#' @describeIn combine_records_helper_functions returns records based on 
#' the index of identical value matching the `match` parameter within the 
#' current column, and collapses them using the provided separator if necessary.
#' @param match_col (character) the name of a column to search for values
#' identical to the `match` parameter.
#' @param match (character) a value to search for in the matching column.
#' @examples
#' 
#' # Select matching records
#' M = combine_records(
#'         group_by = 'example',
#'         default_fcn = .select_match(
#'             match_col = 'match_column',
#'             match = 'find_me',
#'             separator = ', ',
#'             na_string = 'NA')
#'         )
#' @export
.select_exact = function(match_col,match,separator,na_string='NA') {
    fcn = expr(function(x){
        x=x[which(pick(!!match_col)==!!match)]
        
        if (!is.null(!!separator)) {
            x[is.na(x)]=!!na_string
            x=unique(x)
            paste0(x,collapse=!!separator)
        } else {
            return(x)
        }
    })
    return(eval(fcn))
}

#' Unique annotations
#' @describeIn combine_records_helper_functions collapses a set of records to a 
#' set of unique values using the provided separator. `digits` can be provided
#' for numeric columns to control the precision used when determining unique
#' values.
#' @param digits (numeric) the number of digits to use when converting numerical
#' values to characters when determining if values are unique.
#' @examples
#' 
#' # Collapse unique values
#' M = combine_records(
#'         group_by = 'example',
#'         default_fcn = .unique(
#'             digits = 6,
#'             separator = ', ',
#'             na_string = 'NA')
#'         )
#' @export
.unique = function(separator,na_string='NA',digits=6){
    fcn=expr(function(x){
        if (is.numeric(x)) {
            x=round(as.numeric(x),!!digits)
        }
        x[is.na(x)]=!!na_string
        x=unique(x)
        paste0(x,collapse=!!separator)})
    return(eval(fcn))
}

#' Prioritising annotations within a group
#' @describeIn combine_records_helper_functions reduces a set of annotations by 
#' prioritising values according to the input. If there are multiple matches 
#' with the same priority then they are collapsed using a separator.
#' @param match_col (character) the column with labels to prioritise
#' @param priority (character) a list of labels in priority order
#' @param separator (character, NULL) if !NULL this string is used to collapse matches with the same priority
#' @param no_match (character, NULL) if !NULL  then annotations not matching any of the priority labels are replaced with this value
#' @param na_string (character) NA values are replaced with this string  
#' @examples 
#' 
#' # Prioritise by source
#' M = combine_records(
#'         group_by = 'InChiKey',
#'         default_fcn = .prioritise(
#'              match_col = 'source',
#'              priority = c('CD','LS'),
#'              separator = '  || ')
#'     )
#' @export
.prioritise = function(match_col,priority,separator,no_match=NA,na_string='NA'){
    if (!is.list(priority)) {
        # convert to list if not formatted as one
        priority=as.list(priority)
    }
    fcn=expr(function(x){
        w = NULL
        p = !!priority
        for (k in 1:length(p)) {
            w=which(pick(!!match_col)[[1]] %in% p[[k]])
            if (length(w)>0) {break} # stop as soon as we get a match
        }
        # if no matches
        if (length(w)==0){
            # return x unchanged
            if (is.null(!!no_match)){
                return(x)
                # or replace with no_match
            } else {
                return(!!no_match)
            }
        } else {
            x=x[w]
            
            # if separator is not NULL then collapse
            if (!is.null(!!separator))  {
                x[is.na(x)]=!!na_string
                x=unique(x)
                return(paste0(x,collapse=!!separator))
            } else {
                # return all matches
                return(x)
            }
        }
    })
    return(eval(fcn))
}

#' Do nothing to the annotations
#' @describeIn combine_records_helper_functions a pass-through function to 
#' allow some annotation table columns to remain unchanged.
#' @examples 
#' 
#' # Do nothing to all columns
#' M = combine_records(
#'         group_by = 'InChiKey',
#'         default_fcn = .nothing()
#'     )
#' @export
.nothing = function() {
    fcn=expr(function(x){
        return(x)
    })
    return(eval(fcn))
}


#' Number of annotations
#' @describeIn combine_records_helper_functions adds a new column indicating 
#' the number of annotations that match the given grouping variable.
#' @examples 
#' 
#' # Add a column with the number of records with a matching inchikey
#' M = combine_records(
#'         group_by = 'InChiKey',
#'         fcns = list(
#'             count = .count()
#'         ))
#' @export
.count = function() {
    fcn=expr(function(x){
        y=pick(everything())
        return(nrow(y))
    })
    return(eval(fcn))
}

#' Select annotation with the best grade
#' @describeIn combine_records_helper_functions returns records based on the 
#' index of the best grade in a second list. The best grade is defined as "A" 
#' for `upper_case = TRUE` or "a" for `upper_case = FALSE`
#' and the worst grade is "Z" or "z". Any non-exact matches to a character in 
#' `LETTERS` or `letters` are replaced with NA.
#' @param grade_col (character) the name of a column containing grades
#' @param keep_NA (logical) If TRUE keeps records with NA values
#' @param upper_case (logical) If TRUE then grades are compared to upper case
#' letters to determine their ordering, otherwise lower case.
#' @examples
#' 
#' # Select annotation with highest (best) grade
#' M = combine_records(
#'         group_by = 'InChiKey',
#'         default_fcn = .select_grade(
#'             grade_col = 'grade',
#'             keep_NA = FALSE,
#'             upper_case = TRUE
#'         ))
#' @export
.select_grade = function(grade_col,keep_NA=FALSE,upper_case=TRUE) {
    fcn = expr(function(x) {
        # get values
        vals=pick(!!grade_col)
        
        if (!!upper_case){
            vals = match(vals,LETTERS)
        } else {
            vals = match(vals,letters)
        }

        w2=integer()
        if (!all(is.na(vals))) {
            # get index of min
            w  = which.min(vals)
            # find all matches to the min
            w2 = which(vals==min(vals,na.rm=TRUE)) 
        }
        
        # find NA if requested
        if (!!keep_NA) {
            w2=c(w2,which(is.na(vals)))
        }
        
        return(x[w2])
    })
    return(eval(fcn))
}
