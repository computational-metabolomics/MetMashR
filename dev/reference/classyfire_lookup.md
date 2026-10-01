# Query ClassyFire database

Queries the ClassyFire database by inchikey to obtain chemical ontology
information.

## Usage

``` r
classyfire_lookup(
  query_column,
  output_items = "kingdom",
  output_fields = "name",
  suffix = "_cf",
  ...
)
```

## Arguments

- query_column:

  (character) The name of a column in the annotation table containing
  values to search in the api call.

- output_items:

  (character) The names of the items to return from the results of the
  search. Can include any number of "kingdom", "superclass", "class",
  "subclass", "direct_parent", "intermediate_nodes", "substituents",
  "smiles", "molecular_framework", "description", "ancestors",
  "predicted_chebi_terms". Keyword ".all" may be used to return all
  items. The default is `"kingdom"`.

- output_fields:

  (character) The names of fields to return for each output_item. Can
  include any of "name", "description", "chemont_id" and "url". Keyword
  ".all" may be used to return all fields. Some items do not have
  fields, so output_category is ignored. The default is `"name"`.

- suffix:

  (character) A suffix appended to all column names in the returned
  result. The default is `"_cf"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `classyfire_lookup` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The annotation_source after adding data returned by the API. |

## Details

This object makes use of functionality from the following packages:

- `dplyr`

- `httr`

## Inheritance

A `classyfire_lookup` object inherits the following `struct` classes:\
\
`[classyfire_lookup]` -\> `[rest_api]` -\> `[model]` -\>
`[struct_class]`

## References

Wickham H, François R, Henry L, Müller K, Vaughan D (2026). *dplyr: A
Grammar of Data Manipulation*. doi:10.32614/CRAN.package.dplyr
<https://doi.org/10.32614/CRAN.package.dplyr>. R package version 1.2.1,
<https://CRAN.R-project.org/package=dplyr>.

Wickham H (2026). *httr: Tools for Working with URLs and HTTP*.
doi:10.32614/CRAN.package.httr
<https://doi.org/10.32614/CRAN.package.httr>. R package version 1.4.9,
<https://CRAN.R-project.org/package=httr>.

## See also

Other REST API's:
[`classyfire_batch_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/classyfire_batch_lookup.md),
[`cts_lite_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/cts_lite_lookup.md),
[`kegg_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/kegg_lookup.md),
[`lipidmaps_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/lipidmaps_lookup.md),
[`mwb_compound_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/mwb_compound_lookup.md),
[`mwb_refmet_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/mwb_refmet_lookup.md),
[`pubchem_id_exchange()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/pubchem_id_exchange.md),
[`rest_api()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/rest_api.md)

## Examples

``` r
M <- classyfire_lookup(
        output_items = "kingdom",
        output_fields = "name",
        base_url = "http://classyfire.wishartlab.com/entities",
        url_template = "<base_url>/<query_column>.json",
        query_column = character(0),
        cache = NULL,
        cache_mode = "update",
        status_codes = list(),
        delay = 0.5,
        suffix = "_rest_api")
```
