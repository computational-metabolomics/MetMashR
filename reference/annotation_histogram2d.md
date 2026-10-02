# Annotation 2D histogram

Display a histogram of value in the specified columns of an
annotation_source.

## Usage

``` r
annotation_histogram2d(
  factor_name,
  bins = 30,
  bin_edge = "grey",
  bin_fill = "lightgrey",
  vline = NULL,
  vline_colour = "red",
  ...
)
```

## Arguments

- factor_name:

  (character) The names of the two columns in the `annotation_source` to
  generate histograms from.

- bins:

  (numeric, integer) The number of bins to use when computing the
  histograms. The default is `30`.\

- bin_edge:

  (character) The colour to use when plotting the edges of bins. The
  default is `"grey"`.

- bin_fill:

  (character) The colour to use when plotting the bins. The default is
  `"lightgrey"`.

- vline:

  (numeric, NULL, list) The x-axis location of lines used to indicate
  e.g. upper and lower limits. Use NULL if not required. A 2 element
  list can be provided to set vlines for each `factor_name`. The default
  is `NULL`.

- vline_colour:

  (character) The colour to use when plotting vertical lines. The
  default is `"red"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` annotation_histogram2d ` object. This object has no `output` slots.
See [`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in
the `struct` package to plot this chart object.

## Details

This object makes use of functionality from the following packages:

- `ggplot2`

- `patchwork`

## Inheritance

A `annotation_histogram2d` object inherits the following `struct`
classes:\
\
`[annotation_histogram2d]` -\> `[annotation_histogram]` -\> `[chart]`
-\> `[struct_class]`

## References

Wickham H (2016). *ggplot2: Elegant Graphics for Data Analysis*.
Springer-Verlag New York. ISBN 978-3-319-24277-4.
<https://ggplot2.tidyverse.org>.

Pedersen T (2025). *patchwork: The Composer of Plots*.
doi:10.32614/CRAN.package.patchwork
<https://doi.org/10.32614/CRAN.package.patchwork>. R package version
1.3.2, <https://CRAN.R-project.org/package=patchwork>.

## Examples

``` r
M <- annotation_histogram2d(
        factor_name = "V1",
        bins = 30,
        bin_edge = "grey",
        bin_fill = "lightgrey",
        vline = NULL,
        vline_colour = "red")
```
