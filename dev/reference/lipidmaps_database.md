# LIPID MAPS database

Imports the full LIPID MAPS Structure Database (LMSD) bulk compound
export (~50,000 lipids), cached locally with BiocFileCache so it is only
downloaded once rather than queried per-row. Columns include lm_id,
name, abbrev, core, main_class, sub_class, formula, inchi, inchi_key,
kegg_id, hmdb_id, chebi_id, pubchem_cid and smiles – a single
database_lookup() against this table can replace many individual
lipidmaps_lookup() REST queries.

## Usage

``` r
lipidmaps_database(
  bfc_path = NULL,
  resource_name = "MetMashR_lipidmaps",
  source = paste0("https://www.lipidmaps.org/rest/compound/lm_id/LM/all/download"),
  ...
)
```

## Arguments

- bfc_path:

  (character, NULL) `BiocFileCache` is used to cache the database
  locally and prevent unnecessary downloads. If a path is provided then
  `BiocFileCache` will use this location. If NULL it will use the
  default location (see
  [`BiocFileCache::BiocFileCache()`](https://rdrr.io/pkg/BiocFileCache/man/BiocFileCache-class.html)
  for details). The default is `NULL`.

- resource_name:

  (character) The name given to this resource in the cache. (see
  [`BiocFileCache::BiocFileCache()`](https://rdrr.io/pkg/BiocFileCache/man/BiocFileCache-class.html)
  for details). The default is `"MetMashR_lipidmaps"`.

- source:

  (ANY) The source of annotation data. The default is
  `paste0("https://www.lipidmaps.org/rest/compound/lm_id/LM/all/download")`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` lipidmaps_database ` object. This object has no `output` slots.

## Details

This object makes use of functionality from the following packages:

- `BiocFileCache`

## Inheritance

A `lipidmaps_database` object inherits the following `struct` classes:\
\
`[lipidmaps_database]` -\> `[BiocFileCache_database]` -\>
`[annotation_database]` -\> `[annotation_source]` -\> `[struct_class]`

## References

Shepherd L, Morgan M (2026). *BiocFileCache: Manage Files Across
Sessions*. R package version 3.2.0.

## See also

Other database:
[`BiocFileCache_database()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/BiocFileCache_database.md),
[`sqlite_database()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/sqlite_database.md)

## Examples

``` r
M <- lipidmaps_database(
        bfc_path = NULL,
        resource_name = "bfc",
        bfc_fun = function(){},
        import_fun = function(){},
        offline = FALSE,
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
