# Metabolomics Workbench RefMet lookup

Matches a reported compound name (which may be a synonym, conjugate
acid/base form, or otherwise non-canonical name) to its RefMet
standardised name and identifiers, using the Metabolomics Workbench
RefMet REST API. Intended as a substitute for the (now defunct) Chemical
Translation Service (CTS): the original 'PubChem Identifier Exchange +
CTS' translation route described by some published metabolite-merging
methods can no longer be reproduced, since CTS has been shut down and
its replacement, CTS-Lite, does not support name-based lookups. RefMet's
`match` endpoint plays a similar synonym-resolution role, and its
curated picks (e.g. preferring the biologically-relevant
stereoisomer/protonation state over a generic entry) often differ
usefully from a plain PubChem name search.

## Usage

``` r
mwb_refmet_lookup(query_column, suffix = "_refmet", ...)
```

## Arguments

- query_column:

  (character) The name of a column in the annotation table containing
  values to search in the api call.

- suffix:

  (character) A suffix appended to all column names in the returned
  result. The default is `"_refmet"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `mwb_refmet_lookup` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The annotation_source after adding data returned by the API. |

## Details

This object makes use of functionality from the following packages:

- `httr`

## Inheritance

A `mwb_refmet_lookup` object inherits the following `struct` classes:\
\
`[mwb_refmet_lookup]` -\> `[rest_api]` -\> `[model]` -\>
`[struct_class]`

## References

Wickham H (2026). *httr: Tools for Working with URLs and HTTP*.
doi:10.32614/CRAN.package.httr
<https://doi.org/10.32614/CRAN.package.httr>. R package version 1.4.9,
<https://CRAN.R-project.org/package=httr>.

## See also

Other REST API's:
[`classyfire_batch_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/classyfire_batch_lookup.md),
[`classyfire_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/classyfire_lookup.md),
[`cts_lite_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/cts_lite_lookup.md),
[`kegg_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/kegg_lookup.md),
[`lipidmaps_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/lipidmaps_lookup.md),
[`mwb_compound_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_compound_lookup.md),
[`pubchem_id_exchange()`](https://computational-metabolomics.github.io/MetMashR/reference/pubchem_id_exchange.md),
[`rest_api()`](https://computational-metabolomics.github.io/MetMashR/reference/rest_api.md)

## Examples

``` r
M <- mwb_refmet_lookup(
        base_url = "https://www.metabolomicsworkbench.org/rest/refmet",
        url_template = "<base_url>/match/<query_column>/name",
        query_column = character(0),
        cache = NULL,
        cache_mode = "update",
        status_codes = list(),
        delay = 0.5,
        suffix = "_refmet")
```
