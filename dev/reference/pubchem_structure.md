# PubChem molecular structure

Query the PubChem api and retrieve a display an image of the matching
molecular structure.

## Usage

``` r
pubchem_structure(
  query_column,
  search_by,
  row_index,
  record_type = "2d",
  image_size = "large",
  ...
)
```

## Arguments

- query_column:

  (character) The name of the `annotation_source` column with compound
  identifiers of the type specified in the `search_by` param.

- search_by:

  (character) The PubChem domain to search for matches to the
  annotation_column.

- row_index:

  (integer, numeric) The row index of the `annotation_source` to request
  an image of the molecular structure of.

- record_type:

  (character) The record type to return from the PubChem query. Can be
  one of "2d" or "3d". The default is `"2d"`.

- image_size:

  (character) The size of the image to return from the PubChem query.
  Can be one of "large" or "small". For `record_type = "2d"` an
  arbitrary image size can be specified e.g. `123x123`. The default is
  `"large"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` pubchem_structure ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Details

This object makes use of functionality from the following packages:

- `cowplot`

This object queries the PubChem API for matches to your query without
caching the results. It is therefore intended for limited use. If you
wish to obtain images for a large number of moelucules you should seek
an alternative solution.

## Inheritance

A `pubchem_structure` object inherits the following `struct` classes:\
\
`[pubchem_structure]` -\> `[chart]` -\> `[struct_class]`

## References

Wilke C (2025). *cowplot: Streamlined Plot Theme and Plot Annotations
for 'ggplot2'*. doi:10.32614/CRAN.package.cowplot
<https://doi.org/10.32614/CRAN.package.cowplot>. R package version
1.2.0, <https://CRAN.R-project.org/package=cowplot>.

## Examples

``` r
M <- pubchem_structure(
        query_column = "V1",
        search_by = "cid",
        row_index = 1,
        record_type = "2d",
        image_size = "large")
```
