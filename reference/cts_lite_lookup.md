# Batch lookup via CTS-Lite

Uses the CTS-Lite batch REST API to match InChIKeys against a curated
subset of PubChem, returning the matched PubChem entry plus
literature/patent annotation counts.

## Usage

``` r
cts_lite_lookup(
  query_column,
  suffix = "_cts",
  columns = ".all",
  top_hit_only = TRUE,
  first_block_matches = TRUE,
  rdkit_conversion = TRUE,
  cache = NULL,
  cache_mode = "update",
  ...
)
```

## Arguments

- query_column:

  (character) The name of a column in the annotation table containing
  InChIKeys (or InChI, SMILES, molecular formula or PubChem CID values -
  CTS-Lite auto-detects the query type).

- suffix:

  (character) A suffix appended to all column names in the returned
  result. The default is `"_cts"`.

- columns:

  (character) The columns to include in the result. One or more of
  "query_type", "found_match", "match_level", "pubchem_cid", "inchikey",
  "inchi", "smiles", "compound_name", "molecular_formula", "exact_mass",
  "literature_count", "patent_count", "annotation_type_count". Keyword
  ".all" (the default) returns every column. The default is `".all"`.

- top_hit_only:

  (logical) If TRUE (the default), CTS-Lite returns only the single
  best-ranked match per query (by literature/patent count). If FALSE, a
  query may return multiple matches, producing one row per match. The
  default is `TRUE`.\

- first_block_matches:

  (logical) If TRUE (the default), an InChIKey (or converted SMILES)
  query with no exact match may still match on its first 14 characters
  (the connectivity/skeleton block), reported as match_level = "First
  Block". If FALSE, only exact matches are returned. The default is
  `TRUE`.\

- rdkit_conversion:

  (logical) If TRUE (the default), a SMILES query that fails to match
  directly is converted to an InChIKey with RDKit and retried. Has no
  effect for InChIKey queries. The default is `TRUE`.\

- cache:

  (annotation_database, NULL) A struct cache object (e.g.
  [`rds_cache()`](https://computational-metabolomics.github.io/MetMashR/reference/rds_cache.md))
  that stores results of previous CTS-Lite queries, keyed by query
  value. Values already present in the cache are not requeried. If not
  using a cache then set to NULL. The default is `NULL`.

- cache_mode:

  (character) Cache mode. Allowed values are limited to the following:

  - `"update"`: The normal mode: values already in `cache` are used
    as-is, anything missing is queried live and the result added to the
    cache.

  - `"offline"`: Never query the live CTS-Lite API - only values already
    present in `cache` are returned, and everything else is left as NA.
    A warning lists how many query values are not covered by the cache
    when this happens.

  - `"rebuild"`: Ignore any existing cached value and query the live
    CTS-Lite API for every value, overwriting the corresponding entry in
    `cache`.

  The default is `"update"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `cts_lite_lookup` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The annotation_source after adding data returned by CTS-Lite. |

## Details

CTS-Lite (the Fiehn Lab's successor to the original, now-closed Chemical
Translation Service) matches an InChIKey (or InChI, SMILES, molecular
formula or PubChem CID - the query type is auto-detected) against a
curated subset of PubChem, returning the matched PubChem entry plus
literature/patent annotation counts that can be used as a rough
confidence signal. It does not support name-based translation - see
[`mwb_refmet_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_refmet_lookup.md)
or
[`chebi_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/chebi_lookup.md)
for that. Its REST API (`POST .../match`) is batch-based - every query
value is submitted in a single request, rather than one request per
value - so, unlike MetMashR's other REST API lookups, this object is not
built on the `rest_api` base class.

## Inheritance

A `cts_lite_lookup` object inherits the following `struct` classes:\
\
`[cts_lite_lookup]` -\> `[model]` -\> `[struct_class]`

## See also

Other REST API's:
[`classyfire_batch_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/classyfire_batch_lookup.md),
[`classyfire_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/classyfire_lookup.md),
[`kegg_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/kegg_lookup.md),
[`lipidmaps_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/lipidmaps_lookup.md),
[`mwb_compound_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_compound_lookup.md),
[`mwb_refmet_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_refmet_lookup.md),
[`pubchem_id_exchange()`](https://computational-metabolomics.github.io/MetMashR/reference/pubchem_id_exchange.md),
[`rest_api()`](https://computational-metabolomics.github.io/MetMashR/reference/rest_api.md)

## Examples

``` r
M <- cts_lite_lookup(
        query_column = character(0),
        suffix = "_cts",
        columns = ".all",
        top_hit_only = FALSE,
        first_block_matches = FALSE,
        rdkit_conversion = FALSE,
        base_url = "https://cts-lite.metabolomics.us/match",
        cache = NULL,
        cache_mode = "update")
```
