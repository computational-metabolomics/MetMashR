# Convert to/from kegg identifiers

Searches MetabolomicsWorkbench for compound identifiers.

## Usage

``` r
mwb_compound_lookup(
  input_item = "inchi_key",
  query_column,
  output_item = "pubchem_id",
  suffix = "_mwb",
  ...
)
```

## Arguments

- input_item:

  (character) A valid input item for the compound context (see
  https://www.metabolomicsworkbench.org/tools/mw_rest.php). The values
  in the query_column should be of this type. The default is
  `"inchi_key"`.

- query_column:

  (character) The name of a column in the annotation table containing
  values to search in the api call.

- output_item:

  (character) A comma separated list of Valid output items for the
  compound context (see
  https://www.metabolomicsworkbench.org/tools/mw_rest.php). The default
  is `"pubchem_id"`.

- suffix:

  (character) A suffix appended to all column names in the returned
  result. The default is `"_mwb"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `mwb_compound_lookup` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The annotation_source after adding data returned by the API. |

## Details

This object makes use of functionality from the following packages:

- `metabolomicsWorkbenchR`

- `dplyr`

## Inheritance

A `mwb_compound_lookup` object inherits the following `struct` classes:\
\
`[mwb_compound_lookup]` -\> `[rest_api]` -\> `[model]` -\>
`[struct_class]`

## References

Lloyd GR, Weber RJM (2026). *metabolomicsWorkbenchR: Metabolomics
Workbench in R*. R package version 1.22.0.

Wickham H, François R, Henry L, Müller K, Vaughan D (2026). *dplyr: A
Grammar of Data Manipulation*. doi:10.32614/CRAN.package.dplyr
<https://doi.org/10.32614/CRAN.package.dplyr>. R package version 1.2.1,
<https://CRAN.R-project.org/package=dplyr>.

## See also

Other REST API's:
[`classyfire_batch_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/classyfire_batch_lookup.md),
[`classyfire_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/classyfire_lookup.md),
[`cts_lite_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/cts_lite_lookup.md),
[`kegg_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/kegg_lookup.md),
[`lipidmaps_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/lipidmaps_lookup.md),
[`mwb_refmet_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/mwb_refmet_lookup.md),
[`pubchem_id_exchange()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/pubchem_id_exchange.md),
[`rest_api()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/rest_api.md)

## Examples

``` r
M <- mwb_compound_lookup(
        input_item = "inchi_key",
        output_item = "inchi_key",
        base_url = "https://www.metabolomicsworkbench.org/rest",
        url_template = "<base_url>/compound/<input_item>/<query_column>/<output_item>",
        query_column = character(0),
        cache = NULL,
        cache_mode = "update",
        status_codes = list(),
        delay = 0.5,
        suffix = "_rest_api")
```
