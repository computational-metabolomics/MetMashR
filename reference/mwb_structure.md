# MWB molecular structure

Query the Metabolomic Workbench API and retrieve a display an image of
the matching molecular structure.

## Usage

``` r
mwb_structure(query_column, row_index, ...)
```

## Arguments

- query_column:

  (character) The name of the `annotation_source` column with regno
  compound identifiers.

- row_index:

  (integer, numeric) The row index of the `annotation_source` to request
  an image of the molecular structure of.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` mwb_structure ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Details

This object makes use of functionality from the following packages:

- `cowplot`

- `metabolomicsWorkbenchR`

This object queries the Metabolomics Workbench API for matches to your
query without caching the results. It is therefore intended for limited
use. If you wish to obtain images for a large number of molecules you
should seek an alternative solution.

## Inheritance

A `mwb_structure` object inherits the following `struct` classes:\
\
`[mwb_structure]` -\> `[chart]` -\> `[struct_class]`

## References

Wilke C (2025). *cowplot: Streamlined Plot Theme and Plot Annotations
for 'ggplot2'*. doi:10.32614/CRAN.package.cowplot
<https://doi.org/10.32614/CRAN.package.cowplot>. R package version
1.2.0, <https://CRAN.R-project.org/package=cowplot>.

Lloyd GR, Weber RJM (2026). *metabolomicsWorkbenchR: Metabolomics
Workbench in R*. R package version 1.22.0.

## Examples

``` r
M <- mwb_structure(
        row_index = 1,
        query_column = "V1")
```
