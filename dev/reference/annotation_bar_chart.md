# Annotation bar chart

Display a bar chart of labels in the specified column of an
annotation_source.

## Usage

``` r
annotation_bar_chart(
  factor_name,
  label_rotation = FALSE,
  label_location = "inside",
  label_type = "percent",
  legend = FALSE,
  ...
)
```

## Arguments

- factor_name:

  (character) The name of the column in the `annotation_source` to
  generate a chart from.

- label_rotation:

  (logical) Rotate labels. Allowed values are limited to the following:

  - `"TRUE"`: Rotate labels to match segments.

  - `"FALSE"`: Do not rotate labels.

  The default is `FALSE`.\

- label_location:

  (character) Label location. Allowed values are limited to the
  following:

  - `"inside"`: Labels are displayed inside the bars.

  - `"outside"`: Labels are displayed outside the bars.

  The default is `"inside"`.

- label_type:

  (character) Label type. Allowed values are limited to the following:

  - `"percent"`: Labels will include the percentage for the segment.

  - `"count"`: Labels will include the count for the segment.

  - `"none"`: Labels will not include extra information.

  The default is `"percent"`.

- legend:

  (logical) Display legend. Allowed values are limited to the following:

  - `"TRUE"`: Groups are indicated using a legend.

  - `"FALSE"`: Groups are indicated in the labels.

  The default is `FALSE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` annotation_bar_chart ` object. This object has no `output` slots.
See [`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in
the `struct` package to plot this chart object.

## Details

This object makes use of functionality from the following packages:

- `ggplot2`

## Inheritance

A `annotation_bar_chart` object inherits the following `struct`
classes:\
\
`[annotation_bar_chart]` -\> `[chart]` -\> `[struct_class]`

## References

Wickham H (2016). *ggplot2: Elegant Graphics for Data Analysis*.
Springer-Verlag New York. ISBN 978-3-319-24277-4.
<https://ggplot2.tidyverse.org>.

## Examples

``` r
M <- annotation_bar_chart(
        factor_name = "V1",
        label_location = "inside",
        label_rotation = FALSE,
        legend = FALSE,
        label_type = "percent")
```
