#' @eval get_description('lipidmaps_lookup')
#' @export
#' @include annotation_source_class.R rest_api_class.R
#' @family REST API's
lipidmaps_lookup <- function(
        query_column,
        context,
        context_item,
        output_item = "all",
        suffix = "_lipidmaps",
        ...) {
    # check context
    if (!(context %in% c("compound", "gene", "protein"))) {
        stop("context is not valid. See https://lipidmaps.org/resources/rest")
    }
    
    # check context_item
    allowed <- list(
        compound = c(
            "lm_id", "formula", "inchi_key", "pubchem_cid", "hmdb_id",
            "kegg_id", "chebi_id", "smiles", "abbrev", "abbrev_chains"
        ),
        gene = c("lmp_id", "gene_id", "gene_name", "gene_symbol", "tax_id"),
        protein = c(
            "lmp_id", "gene_id", "gene_name", "gene_symbol", "tax_id",
            "mrna_id", "refseq_id", "protein_gi", "protein_entry",
            "protein_name", "uniprot_id"
        )
    )
    
    if (!all(context_item %in% allowed[[context]])) {
        stop(
            "A context_item is not valid for the current context. ",
            "See https://lipidmaps.org/resources/rest"
        )
    }
    
    # check output_item
    allowed <- list(
        compound = c(
            "input", "regno", "lm_id", "name", "sys_name", "synonyms",
            "abbrev", "abbrev_chains", "core", "main_class", "sub_class",
            "exactmass", "formula", "inchi", "inchi_key", "hmdb_id",
            "chebi_id", "pubchem_cid", "smiles", "kegg_id", "all"
        ),
        gene = c(
            "gene_id", "lmp_id", "gene_name", "gene_symbol", "gene_synonyms",
            "chromosome", "map_location", "summary", "taxid", "species",
            "species_long", "all"
        ),
        protein = c(
            "uniprot_id", "lmp_id", "gene_id", "gene_name", "gene_symbol",
            "taxid", "species", "species_long", "mrna_id", "refseq_id",
            "protein_gi", "protein_entry", "protein_name", "seq_length",
            "seq", "all"
        )
    )
    
    
    if (length(output_item) > 1) {
        if (!all(output_item %in% allowed[[context]])) {
            stop(
                "An output_item is not valid for the current context. ",
                "See https://lipidmaps.org/resources/rest"
            )
        }
        # collapse if length >1
        output_item <- paste0(output_item, collapse = ",")
    }
    
    out <- struct::new_struct(
        "lipidmaps_lookup",
        query_column = query_column,
        context = context,
        context_item = context_item,
        suffix = suffix,
        output_item = output_item,
        ...
    )
    return(out)
}



.lipidmaps_lookup <- setClass(
    "lipidmaps_lookup",
    contains = c("rest_api"),
    slots = c(
        query_column = "entity",
        context = "entity",
        updated = "entity",
        context_item = "entity",
        output_item = "entity"
    ),
    prototype = list(
        name = "LipidMaps api lookup",
        description = paste0("Search the LipidMaps database using the API"),
        type = "univariate",
        predicted = "updated",
        .params = c("query_column", "output_item", "context", "context_item"),
        .outputs = c("updated"),
        base_url = entity(
            name = "Base URL",
            description = "The base URL of the API.",
            type = c("character"),
            value = "https://www.lipidmaps.org/rest",
            max_length = 1
        ),
        url_template = entity(
            name = "URL template",
            description = paste0(
                "A template describing how the URL should be ",
                "constructed from the base URL and input parameters. e.g. ",
                "<base_url>/<context>/<input_item>/<search_term>/json.",
                "The url will be constructed by replacing the values ",
                "enclosed in <> with the value from corresponding input ",
                "parameter of the rest_api object."
            ),
            value = paste0(
                "<base_url>/<context>/<context_item>/",
                "<query_column>/<output_item>/json"
            ),
            max_length = 1
        ),
        output_item = entity(
            name = "Output item",
            description = paste0(
                "The names of the columns to return from the results of the ",
                "search. See https://lipidmaps.org/resources/rest for details."
            ),
            type = c("character"),
            value = "input",
            max_length = 1
        ),
        context = enum(
            name = "Search context",
            description = paste0(
                'The search API context. Must be one of "compound", "gene", ',
                'or "protein"'
            ),
            type = "character",
            max_length = 1,
            value = "compound",
            allowed = c("compound", "gene", "protein")
        ),
        context_item = entity(
            name = "Context item",
            description = paste0(
                "The context item being searched. See ",
                "https://lipidmaps.org/resources/rest for details."
            ),
            type = "character",
            max_length = Inf
        ),
        status_codes = entity(
            name = "Status codes",
            description = paste0(
                "Named list of status codes and function indicating how to ",
                "respond. Should minimally contain a function to parse a ",
                "successful response for status code 200. Any codes not ",
                "provided will be passed to httr::stop_for_status()."
            ),
            type = "list",
            value = list(
                "200" = .parse_json_lipidmaps,
                "403" = function(...) {
                    return(NULL)
                },
                "404" = function(...) {
                    return(NULL)
                }
            ),
            max_length = Inf
        )
    )
)
