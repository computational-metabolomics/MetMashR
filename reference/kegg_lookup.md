# Convert to or from kegg identifiers

Searches the Kegg database to obtain external identifiers. KEGG
compound, drug and glycan databases can be queried for pubchem and chebi
identifiers, and vice-versa.

## Usage

``` r
kegg_lookup(
  get = "pubchem_sid",
  from = "compound",
  query_column,
  suffix = "_kegg",
  cache = NULL,
  cache_mode = "update",
  ...
)
```

## Arguments

- get:

  (character) Get identifier. Allowed values are limited to the
  following:

  - `"compound"`: KEGG small molecule database.

  - `"glycan"`: KEGG glycan database.

  - `"drug"`: KEGG drug database.

  - `"chebi"`: Chemical Entities of Biological Interest (ChEBI)
    database.

  - `"pubchem_sid"`: PubChem Substance Identifier.

  The default is `"pubchem_sid"`.

- from:

  (character) From identifier. Allowed values are limited to the
  following:

  - `"compound"`: KEGG small molecule database.

  - `"glycan"`: KEGG glycan database.

  - `"drug"`: KEGG drug database.

  - `"chebi"`: Chemical Entities of Biological Interest (ChEBI)
    database.

  - `"pubchem_sid"`: PubChem Substance Identifier.

  The default is `"compound"`.

- query_column:

  (character) The name of the column containing identifiers to search
  the database for. They should be identifiers of the type selected for
  the "from" slot.

- suffix:

  (character) A suffix appended to all column names in the returned
  result. The default is `"_kegg"`.

- cache:

  (annotation_database, NULL) A struct cache object (e.g.
  [`rds_cache()`](https://computational-metabolomics.github.io/MetMashR/reference/rds_cache.md))
  that stores results of previous KEGG conversions, keyed by query
  value. Values already present in the cache are not requeried. If not
  using a cache then set to NULL. The default is `NULL`.

- cache_mode:

  (character) Cache mode. Allowed values are limited to the following:

  - `"update"`: The normal mode: values already in `cache` are used
    as-is, anything missing is queried live and the result added to the
    cache.

  - `"offline"`: Never query the live KEGG API. Only values already
    present in `cache` are returned, and everything else is left as NA.

  - `"rebuild"`: Ignore any existing cached value and query the live
    KEGG API for every value, overwriting the corresponding entry in
    `cache`. Useful when cached results are known to be stale.

  The default is `"update"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `kegg_lookup` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) An annotation_source object with a new column of compound identifiers. |

## Details

This object makes use of functionality from the following packages:

- `KEGGREST`

- `dplyr`

## Inheritance

A `kegg_lookup` object inherits the following `struct` classes:\
\
`[kegg_lookup]` -\> `[model]` -\> `[struct_class]`

## References

Tenenbaum D, Maintainer B (2026). *KEGGREST: Client-side REST access to
the Kyoto Encyclopedia of Genes and Genomes (KEGG)*.
doi:10.18129/B9.bioc.KEGGREST
<https://doi.org/10.18129/B9.bioc.KEGGREST>. R package version 1.52.2,
<https://bioconductor.org/packages/KEGGREST>.

Wickham H, François R, Henry L, Müller K, Vaughan D (2026). *dplyr: A
Grammar of Data Manipulation*. doi:10.32614/CRAN.package.dplyr
<https://doi.org/10.32614/CRAN.package.dplyr>. R package version 1.2.1,
<https://CRAN.R-project.org/package=dplyr>.

## See also

Other REST API's:
[`classyfire_batch_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/classyfire_batch_lookup.md),
[`classyfire_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/classyfire_lookup.md),
[`cts_lite_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/cts_lite_lookup.md),
[`lipidmaps_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/lipidmaps_lookup.md),
[`mwb_compound_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_compound_lookup.md),
[`mwb_refmet_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_refmet_lookup.md),
[`pubchem_id_exchange()`](https://computational-metabolomics.github.io/MetMashR/reference/pubchem_id_exchange.md),
[`rest_api()`](https://computational-metabolomics.github.io/MetMashR/reference/rest_api.md)

## Examples

``` r
M <- kegg_lookup(
        get = "pubchem_sid",
        from = "compound",
        query_column = "V1",
        suffix = "_kegg",
        cache = NULL,
        cache_mode = "update")
```
