# Annotation histogram

Display a histogram of value in the specified column of an
annotation_source.

## Usage

``` r
annotation_histogram(
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

  (character) The name of the column in the `annotation_source` to
  generate a histogram from.

- bins:

  (numeric, integer) The number of bins to use when computing the
  histogram. The default is `30`.\

- bin_edge:

  (character) The colour to use when plotting the edges of bins. The
  default is `"grey"`.

- bin_fill:

  (character) The colour to use when plotting the bins. The default is
  `"lightgrey"`.

- vline:

  (numeric, NULL, list) The x-axis location of veritcal lines used to
  indicate e.g. upper and lower limits. Use NULL if not required. The
  default is `NULL`.

- vline_colour:

  (character) The colour to use when plotting vertical lines. The
  default is `"red"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` annotation_histogram ` object. This object has no `output` slots.
See [`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in
the `struct` package to plot this chart object.

## Details

This object makes use of functionality from the following packages:

- `ggplot2`

## Inheritance

A `annotation_histogram` object inherits the following `struct`
classes:\
\
`[annotation_histogram]` -\> `[chart]` -\> `[struct_class]`

## References

Wickham H (2016). *ggplot2: Elegant Graphics for Data Analysis*.
Springer-Verlag New York. ISBN 978-3-319-24277-4.
<https://ggplot2.tidyverse.org>.

## Examples

``` r
M <- annotation_histogram(
        factor_name = "V1",
        bins = 30,
        bin_edge = "grey",
        bin_fill = "lightgrey",
        vline = NULL,
        vline_colour = "red")
```
