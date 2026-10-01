# small helper: return b if a is NULL, otherwise a
.chebi_default <- function(a, b) {
    if (is.null(a)) b else a
}

# small helper: keep only the requested columns (in the order supplied),
# silently dropping any that don't apply to the current search_by direction
# (e.g. "name" when search_by = "chebi_id"). ".all"/NULL keeps everything.
.chebi_select_columns <- function(out, columns) {
    if (is.null(columns) || any(columns == ".all")) {
        return(out)
    }
    keep <- intersect(columns, colnames(out))
    out[, keep, drop = FALSE]
}

# internal function to parse ChEBI API json responses. Handles both the
# es_search (name -> id) and compound/<id> (id -> synonym) response shapes.
.parse_chebi_lookup <- function(response, params) {
    response <- httr::content(response, as = "text", encoding = "UTF-8")
    J <- jsonlite::fromJSON(response, simplifyVector = FALSE)

    if (identical(params$search_by, "name")) {
        # es_search response: {"results": [{"_source": {...}}, ...]}. Each
        # hit's _source already carries inchikey/smiles/inchi/formula/mass/
        # charge/stars directly (alongside chebi_accession/name), so a
        # separate compound/<id> call is not needed just to get the
        # structure for a name search.
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
                inchikey = .chebi_default(src$inchikey, NA_character_),
                smiles = .chebi_default(src$smiles, NA_character_),
                inchi = .chebi_default(src$inchi, NA_character_),
                formula = .chebi_default(src$formula, NA_character_),
                mass = as.character(.chebi_default(src$mass, NA_character_)),
                monoisotopicmass = as.character(
                    .chebi_default(src$monoisotopicmass, NA_character_)
                ),
                charge = as.character(.chebi_default(src$charge, NA_character_)),
                stars = as.integer(.chebi_default(src$stars, NA_integer_)),
                stringsAsFactors = FALSE
            )
        })
        out <- plyr::rbind.fill(out)

        if (identical(params$records, "best")) {
            out <- out[1, , drop = FALSE]
        }
        return(.chebi_select_columns(out, params$columns))
    }

    if (identical(params$search_by, "chebi_id")) {
        # compound/<id> response: a single entity. Synonyms are nested
        # under names$SYNONYM (a list of {name, ascii_name, ...}), not a
        # top-level "synonyms" array. Structural identifiers (InChIKey,
        # SMILES, InChI) live under default_structure, and formula/mass/
        # charge under chemical_data.
        chebi_id <- .chebi_default(
            J$chebi_accession,
            paste0("CHEBI:", .chebi_default(J$id, NA_character_))
        )
        primary_name <- .chebi_default(J$ascii_name, J$name)

        synonyms <- J$names$SYNONYM
        synonym_names <- character(0)
        if (!is.null(synonyms) && length(synonyms) > 0) {
            synonym_names <- vapply(synonyms, function(s) {
                if (is.list(s)) {
                    as.character(.chebi_default(s$ascii_name, s$name))
                } else {
                    as.character(s)
                }
            }, character(1))
        }

        all_names <- unique(c(primary_name, synonym_names))
        all_names <- all_names[!is.na(all_names)]

        struct_ <- J$default_structure
        chem <- J$chemical_data
        inchikey <- .chebi_default(struct_$standard_inchi_key, NA_character_)
        smiles <- .chebi_default(struct_$smiles, NA_character_)
        inchi <- .chebi_default(struct_$standard_inchi, NA_character_)
        formula <- .chebi_default(chem$formula, NA_character_)
        mass <- as.character(.chebi_default(chem$mass, NA_character_))
        monoisotopicmass <- as.character(
            .chebi_default(chem$monoisotopic_mass, NA_character_)
        )
        charge <- as.character(.chebi_default(chem$charge, NA_character_))
        stars <- as.integer(.chebi_default(J$stars, NA_integer_))

        if (length(all_names) == 0) {
            return(NA)
        }

        if (identical(params$records, "best")) {
            out <- data.frame(
                chebi_id = chebi_id,
                synonym = .chebi_default(primary_name, all_names[1]),
                inchikey = inchikey,
                smiles = smiles,
                inchi = inchi,
                formula = formula,
                mass = mass,
                monoisotopicmass = monoisotopicmass,
                charge = charge,
                stars = stars,
                stringsAsFactors = FALSE
            )
        } else {
            out <- data.frame(
                chebi_id = chebi_id,
                synonym = all_names,
                inchikey = inchikey,
                smiles = smiles,
                inchi = inchi,
                formula = formula,
                mass = mass,
                monoisotopicmass = monoisotopicmass,
                charge = charge,
                stars = stars,
                stringsAsFactors = FALSE
            )
        }
        return(.chebi_select_columns(out, params$columns))
    }

    return(NA)
}
