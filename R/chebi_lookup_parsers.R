# small helper: return b if a is NULL, otherwise a
.chebi_default <- function(a, b) {
    if (is.null(a)) b else a
}

# internal function to parse ChEBI API json responses. Handles both the
# es_search (name -> id) and compound/<id> (id -> synonym) response shapes.
.parse_chebi_lookup <- function(response, params) {
    response <- httr::content(response, as = "text", encoding = "UTF-8")
    J <- jsonlite::fromJSON(response, simplifyVector = FALSE)

    if (identical(params$search_by, "name")) {
        # es_search response: {"results": [{"_source": {...}}, ...]}
        hits <- J$results
        if (is.null(hits) || length(hits) == 0) {
            return(NA)
        }

        out <- lapply(hits, function(h) {
            src <- .chebi_default(h[["_source"]], h)
            data.frame(
                chebi_id = .chebi_default(src$chebi_accession, NA_character_),
                name = .chebi_default(
                    .chebi_default(src$ascii_name, src$name),
                    NA_character_
                ),
                stringsAsFactors = FALSE
            )
        })
        out <- plyr::rbind.fill(out)

        if (identical(params$records, "best")) {
            out <- out[1, , drop = FALSE]
        }
        return(out)
    }

    if (identical(params$search_by, "chebi_id")) {
        # compound/<id> response: a single entity, with a "synonyms" list
        chebi_id <- .chebi_default(
            J$chebi_accession,
            paste0("CHEBI:", .chebi_default(J$id, NA_character_))
        )
        primary_name <- .chebi_default(J$ascii_name, J$name)

        synonyms <- J$synonyms
        synonym_names <- character(0)
        if (!is.null(synonyms) && length(synonyms) > 0) {
            synonym_names <- vapply(synonyms, function(s) {
                if (is.list(s)) {
                    as.character(.chebi_default(s$name, s$data))
                } else {
                    as.character(s)
                }
            }, character(1))
        }

        all_names <- unique(c(primary_name, synonym_names))
        all_names <- all_names[!is.na(all_names)]

        if (length(all_names) == 0) {
            return(NA)
        }

        if (identical(params$records, "best")) {
            return(data.frame(
                chebi_id = chebi_id,
                synonym = .chebi_default(primary_name, all_names[1]),
                stringsAsFactors = FALSE
            ))
        }

        return(data.frame(
            chebi_id = chebi_id,
            synonym = all_names,
            stringsAsFactors = FALSE
        ))
    }

    return(NA)
}
