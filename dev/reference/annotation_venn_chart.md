# Annotation venn chart

Display a venn diagram of labels present in two annotation_sources.

## Usage

``` r
annotation_venn_chart(
  factor_name,
  group_column = NULL,
  fill_colour = "white",
  line_colour = "black",
  labels = TRUE,
  legend = FALSE,
  ...
)
```

## Arguments

- factor_name:

  (character) The name of the column(s) in the `annotation_source` to
  generate a chart from. Up to seven columns can be compared for a
  single `annotation_source`.

- group_column:

  (character, NULL) The name of the column in the `annotation_source` to
  create groups from in the Venn diagram. This parameter is ignored if
  there are multiple input tables, as each table is considered to be a
  group. This parameter is also ignored if more than one `factor_name`
  is provided, as each column is considered a group. The default is
  `NULL`.

- fill_colour:

  (character) The line colour of the groups in a format compatible with
  ggplot e.g. "black" or "#000000". Special case ".group" sets the
  colour based on the group label and "none" will not fill the groups.
  The default is `"white"`.

- line_colour:

  (character) The line colour of the groups in a format compatible with
  ggplot e.g. "black" or "#000000". Special case ".group" sets the
  colour based on the group label, and ".none" will not display lines.
  The default is `"black"`.

- labels:

  (logical) Group labels. Allowed values are limited to the following:

  - `"TRUE"`: Include group labels on the plot.

  - `"FALSE"`: Do not inlude group labels on the plot.

  The default is `TRUE`.\

- legend:

  (logical) Legend. Allowed values are limited to the following:

  - `"TRUE"`: Include a legend in the plot.

  - `"FALSE"`: Do not inlude a legend in the plot.

  The default is `FALSE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` annotation_venn_chart ` object. This object has no `output` slots.
See [`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in
the `struct` package to plot this chart object.

## Details

This object makes use of functionality from the following packages:

- `ggVennDiagram`

## Inheritance

A `annotation_venn_chart` object inherits the following `struct`
classes:\
\
`[annotation_venn_chart]` -\> `[chart]` -\> `[struct_class]`

## References

Gao C, Dusa A (2026). *ggVennDiagram: A 'ggplot2' Implement of Venn
Diagram*. doi:10.32614/CRAN.package.ggVennDiagram
<https://doi.org/10.32614/CRAN.package.ggVennDiagram>. R package version
1.5.7, <https://CRAN.R-project.org/package=ggVennDiagram>.

## Examples

``` r
M <- annotation_venn_chart(
        factor_name = "V1",
        line_colour = ".group",
        fill_colour = ".group",
        labels = FALSE,
        legend = FALSE,
        group_column = NULL)
```
