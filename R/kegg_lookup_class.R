#' @eval get_description('kegg_lookup')
#' @export
#' @include annotation_source_class.R
#' @family REST API's
kegg_lookup <- function(get = "pubchem",
    from = "compound",
    query_column,
    suffix = "_kegg",
    ...) {
    # check for suitable combinations of get and from
    if (get %in% c("compound", "drug", "glycan") &
        from %in% c("compound", "drug", "glycan")) {
        stop(
            "compound, drug and glycan ids can only be converted to chebi ",
            "or pubchem ids"
        )
    }
    if (get %in% c("pubchem", "chebi") &
        from %in% c("pubchem", "chebi")) {
        stop(
            "pubchem and chebi ids can only be converted to compound, drug ",
            "or glycan ids"
        )
    }

    out <- struct::new_struct(
        "kegg_lookup",
        get = get,
        from = from,
        query_column = query_column,
        suffix = suffix,
        ...
    )
    return(out)
}

.kegg_lookup <- setClass(
    "kegg_lookup",
    contains = c("model"),
    slots = c(
        get = "enum",
        from = "enum",
        query_column = "entity",
        updated = "entity",
        suffix = "entity"
    ),
    prototype = list(
        name = "Convert to or from kegg identifiers",
        description = paste0(
            "Searches the Kegg database to obtain external ",
            "identifiers. KEGG compound, drug and glycan databases can be ",
            "queried for pubchem and chebi identifiers, and vice-versa."
        ),
        type = "kegg_api",
        predicted = "updated",
        libraries = c("KEGGREST", "dplyr"),
        .params = c("get", "from", "query_column", "suffix"),
        .outputs = c("updated"),
        query_column = entity(
            name = "From column name",
            description = paste0(
                "The name of the column containing ",
                "identifiers to search the database for. They should be ",
                'identifiers of the type selected for the "from" slot.'
            ),
            type = c("character"),
            value = "V1",
            max_length = 1
        ),
        get = enum(
            name = "Get identifier",
            description = c(
                "compound" = "KEGG small molecule database",
                "glycan" = "KEGG glycan database",
                "drug" = "KEGG drug database",
                "chebi" = paste0(
                    "Chemical Entities of Biological Interest (ChEBI) database"
                ),
                "pubchem" = "PubChem Substance Identifier"
            ),
            type = "character",
            max_length = 1,
            allowed = c("compound", "glycan", "drug", "chebi", "pubchem"),
            value = "pubchem"
        ),
        from = enum(
            name = "From identifier",
            description = c(
                "compound" = "KEGG small molecule database",
                "glycan" = "KEGG glycan database",
                "drug" = "KEGG drug database",
                "chebi" = paste0(
                    "Chemical Entities of Biological Interest (ChEBI) database"
                ),
                "pubchem" = "PubChem Substance Identifier"
            ),
            type = "character",
            max_length = 1,
            allowed = c("compound", "glycan", "drug", "chebi", "pubchem"),
            value = "compound"
        ),
        updated = entity(
            name = "Updated annotations",
            description = paste0(
                "An annotation_source object with a new ",
                "column of compound identifiers"
            ),
            type = "annotation_source",
            max_length = Inf
        ),
        suffix = entity(
            name = "Column name suffix",
            description = paste0(
                "A suffix appended to all column names in ",
                "the returned result."
            ),
            value = "_kegg",
            type = "character",
            max_length = 1
        )
    )
)


#' @export
#' @template model_apply
setMethod(
    f = "model_apply",
    signature = c("kegg_lookup", "annotation_source"),
    definition = function(M, D) {
        # check for 0 annotations
        if (nrow(D$data) == 0) {
            # add column
            D$data[[paste0(M$get, M$suffix)]] <- character(0)
            # nothing to do, so return
            M$updated <- D
            return(M)
        }

        # get source column
        src <- as.character(D$data[[M$query_column]])

        # add from str
        src_str <- paste(M$from, src, sep = ":")

        # query kegg
        result <- KEGGREST::keggConv(
            target = M$get,
            source = src_str,
            querySize = 100
        )

        # convert to data.frame
        df <- data.frame(from = names(result), get = result)

        # extract ids
        df <- lapply(df, function(x) {
            y <- strsplit(x, ":", fixed = TRUE)
            y <- unlist(lapply(y, "[", i = 2))
            return(y)
        })

        df <- as.data.frame(df)

        if (nrow(df) == 0) {
            df <- data.frame(from = character(0), to = character(0))
        }

        colnames(df) <- c(M$query_column, M$get)
        colnames(df) <- paste0(colnames(df), M$suffix)
        by <- paste0(M$query_column, M$suffix)
        names(by) <- M$query_column

        # left join with annotations (keggConv excludes ids with no hit)
        X <- D$data
        X[[M$query_column]] <- as.character(X[[M$query_column]])

        X <- dplyr::left_join(X, df, by = by)

        # update
        D$data <- X
        M$updated <- D

        # return
        return(M)
    }
)
