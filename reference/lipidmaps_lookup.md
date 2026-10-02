# LipidMaps api lookup

Search the LipidMaps database using the API

## Usage

``` r
lipidmaps_lookup(
  query_column,
  context,
  context_item,
  output_item = "all",
  suffix = "_lipidmaps",
  ...
)
```

## Arguments

- query_column:

  (character) The name of a column in the annotation table containing
  values to search in the api call.

- context:

  (character) The search API context. Must be one of "compound", "gene",
  or "protein".

- context_item:

  (character) The context item being searched. See
  https://lipidmaps.org/resources/rest for details.

- output_item:

  (character) The names of the columns to return from the results of the
  search. See https://lipidmaps.org/resources/rest for details. The
  default is `"all"`.

- suffix:

  (character) A suffix appended to all column names in the returned
  result. The default is `"_lipidmaps"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `lipidmaps_lookup` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The annotation_source after adding data returned by the API. |

## Inheritance

A `lipidmaps_lookup` object inherits the following `struct` classes:\
\
`[lipidmaps_lookup]` -\> `[rest_api]` -\> `[model]` -\> `[struct_class]`

## See also

Other REST API's:
[`classyfire_batch_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/classyfire_batch_lookup.md),
[`classyfire_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/classyfire_lookup.md),
[`cts_lite_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/cts_lite_lookup.md),
[`kegg_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/kegg_lookup.md),
[`mwb_compound_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_compound_lookup.md),
[`mwb_refmet_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_refmet_lookup.md),
[`pubchem_id_exchange()`](https://computational-metabolomics.github.io/MetMashR/reference/pubchem_id_exchange.md),
[`rest_api()`](https://computational-metabolomics.github.io/MetMashR/reference/rest_api.md)

## Examples

``` r
M <- lipidmaps_lookup(
        query_column = character(0),
        output_item = "input",
        context = "compound",
        context_item = character(0),
        base_url = "https://www.lipidmaps.org/rest",
        url_template = "<base_url>/<context>/<context_item>/<query_column>/<output_item>/json",
        cache = NULL,
        cache_mode = "update",
        status_codes = list(),
        delay = 0.5,
        suffix = "_rest_api")
```
