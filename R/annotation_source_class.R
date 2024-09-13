#' @eval get_description("annotation_database")
#' @include zzz.R generics.R
#' @family {annotation databases}
#' @export
#' @import methods
annotation_source <- function(
        source = character(0),
        data = data.frame(),
        tag = "",
        ...) {
    # new object
    out <- new_struct(
        "annotation_source",
        source = source,
        data = data,
        tag = tag,
        ...
    )
    
    return(out)
}

.annotation_source <- setClass(
    "annotation_source",
    contains = c("struct_class"),
    slots = c(
        source = "entity",
        data = "entity",
        tag = "entity",
        .required = "character"
    ),
    prototype = list(
        name = "An annotation source",
        description = paste0(
            "A base class defining an annotation source. ",
            "This object is extended by MetmashR to define other objects."
        ),
        data = entity(
            name = "Data",
            description = "A data.frame of annotation data.",
            type = c("data.frame", "NULL"),
            value = data.frame()
        ),
        tag = entity(
            name = "Annotation source id tag.",
            description = paste0(
                "A (short) character string that is used to ",
                "represent this source e.g. in column names or source ",
                "columns when used in a workflow."
            ),
            type = c("character"),
            max_length = 1
        ),
        source = entity(
            name = "Annotation source",
            description = paste0(
                "The source of annotation data."
            ),
            type = "ANY",
            max_length = Inf,
        ),
        .params = c("tag", "data", "source")
    ),
)




#' @export
setMethod(
    f = "model_apply",
    signature = c("model", "annotation_source"),
    definition = function(M, D) {
        warning(
            'No model_apply method defined for "',
            class(M)[1], '" objects.'
        )
        return(M)
    }
)

#' @export
setMethod(
    f = "model_apply",
    signature = c("model", "list"),
    definition = function(M, D) {
        Z <- lapply(D, function(x) {
            m <- model_apply(M, x)
            return(m)
        })
        return(Z)
    }
)

#' @export
setMethod(
    f = "model_apply",
    signature = c("model_seq", "list"),
    definition = function(M, D) {
        Z <- lapply(D, function(x) {
            m <- model_apply(M, x)
            return(m)
        })
        return(Z)
    }
)

#' @export
setMethod(
    f = "model_apply",
    signature = c("model_seq", "annotation_source"),
    definition = function(M, D) {
        # for each method in the list
        S <- D # for first in list the input D is the data object
        
        for (i in seq_len(length(M))) {
            if (M[i]@seq_in != "data") {
                # apply transformation
                S <- M[i]@seq_fcn(S)
                # set
                param_value(M[i], M[i]@seq_in) <- S
            }
            # use current data
            M[i] <- model_apply(M[i], D)
            
            # set the output of this method as the input for the next method
            S <- predicted(M[i])
            if (is(S, "annotation_source")) {
                # if its a dataset then update current D
                D <- predicted(M[i])
            }
            # if D is empty, then no annotations
            if (nrow(D$data) == 0) {
                warning(
                    "Model sequence did not continue after step ", class(M[i]),
                    " because the annotation table is empty."
                )
                break
            }
        }
        return(M)
    }
)

#' @export
setMethod(
    f = "show",
    signature = c("annotation_source"),
    definition = function(object) {
        # print struct generic info
        callNextMethod()
        
        cat("annotations:   ", nrow(object$data), " rows x ",
            ncol(object$data), " columns\n",
            sep = ""
        )
        
        utils::head(object$data)
    }
)

#' @export
setMethod(
    f = "read_source",
    signature = c("annotation_source"),
    definition = function(obj) {
        warning(
            'No read_source method defined for "', class(obj)[1],
            '" objects.'
        )
        return(obj)
    }
)

#' @export
setMethod(
    f = "check_for_columns",
    signature = c("annotation_source"),
    definition = function(obj, ..., msg = FALSE) {
        L <- unlist(list(...))
        
        w <- which(!(L %in% colnames(obj$data)))
        
        if (length(w) == 0) {
            return(TRUE)
        }
        
        if (msg) {
            msg <- paste0(
                "The following columns are missing from the ",
                "data.frame: ",
                paste0('"', L[w], '"', collapse = " ,")
            )
            
            if (!is.null(names(L))) {
                msg <- c(
                    msg,
                    paste0(
                        'The column named in the "', names(L)[w], '"',
                        " parameter must be ",
                        "present in the data.frame."
                    )
                )
            }
            
            return(msg)
        } else {
            return(FALSE)
        }
    }
)



#' @export
setMethod(
    f = "vertical_join",
    signature = c("annotation_source", "annotation_source"),
    definition = function(
        x, 
        y, 
        matching_columns = NULL, 
        keep_cols = NULL,
        source_col = "annotation_source",
        exclude_cols = NULL, 
        as = annotation_source()) {
        
        xd <- x$data
        yd <- y$data
        
        # rename columns
        if (!is.null(matching_columns)) {
            xd <- x$data %>% rename(any_of(matching_columns))
            yd <- y$data %>% rename(any_of(matching_columns))
        }
        
        # add source columns
        if (nrow(xd) > 0) {
            xd[[source_col]] <- x$tag
        }
        if (nrow(yd) > 0) {
            yd[[source_col]] <- y$tag
        }
        
        # bind
        zd <- plyr::rbind.fill(xd, yd)
        
        # select columns
        if (length(keep_cols) > 0) {
            if (keep_cols[1] == ".all") {
                keep_cols <- colnames(zd)
            }
        }
        if (length(exclude_cols) > 0) {
            w <- which(keep_cols %in% exclude_cols)
            if (length(w) > 0) {
                keep_cols <- keep_cols[-w]
            }
        }
        # ensure required columns are retained
        keep_cols <- unique(
            c(
                names(matching_columns),
                keep_cols,
                required_cols(x),
                required_cols(y),
                source_col
            )
        )
        
        zd <- zd %>% select(any_of(keep_cols))
        
        # update provided object
        OUT <- as
        OUT$data <- zd
        return(OUT)
    }
)

#' @export
setMethod(
    f = "vertical_join",
    signature = c("list", "missing"),
    definition = function(
        x, 
        y, 
        matching_columns = NULL, 
        keep_cols = NULL,
        source_col = "annotation_source",
        exclude_cols = NULL, as = annotation_source()) {
        
        A <- x
        
        J <- A[[1]]
        AT <- rep(J$tag, nrow(J$data))
        for (k in 2:length(A)) {
            B <- A[[k]]
            C <- vertical_join(
                x = J,
                y = B,
                matching_columns = matching_columns,
                keep_cols = keep_cols,
                source_col = source_col,
                exclude_cols = exclude_cols,
                as = as
            )
            
            AT_new <- C$data[[source_col]]
            AT_new[seq_len(length(AT))] <- AT
            AT <- AT_new
            
            J <- C
        }
        J$data[[source_col]] <- AT
        return(J)
    }
)

.has_required <- function(object) {
    # return if NULL
    if (is.null(object$data)) {
        return(TRUE)
    }
    
    # otherwise, check for columns
    req <- object@.required
    
    check <- is.null(req) | all(req %in% colnames(object$data))
    
    msg <- TRUE
    if (!check) {
        msg <- paste0(
            'Column(s) named "', paste0(req, collapse = '","'), '" ',
            "must be present in the data.frame for this source."
        )
    }
    return(msg)
}

setValidity(
    "annotation_source",
    function(object) {
        msg <- .has_required(object)
        return(msg)
    }
)

#' @export
setMethod(
    f = "required_cols",
    signature = c("annotation_source"),
    definition = function(obj) {
        return(obj@.required)
    }
)
