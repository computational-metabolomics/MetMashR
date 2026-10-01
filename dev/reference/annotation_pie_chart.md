# Annotation pie chart

Display a pie chart of labels in the specified column of an
annotation_source.

## Usage

``` r
annotation_pie_chart(
  factor_name,
  label_rotation = FALSE,
  label_location = "inside",
  label_type = "percent",
  legend = FALSE,
  pie_rotation = 0,
  centre_radius = 0,
  centre_label = NULL,
  count_na = FALSE,
  ...
)
```

## Arguments

- factor_name:

  (character) The name of the column in the `annotation_source` to
  generate a pie chart from.

- label_rotation:

  (logical) Rotate labels. Allowed values are limited to the following:

  - `"TRUE"`: Rotate labels to match segments.

  - `"FALSE"`: Do not rotate labels.

  The default is `FALSE`.\

- label_location:

  (character) Label location. Allowed values are limited to the
  following:

  - `"inside"`: Labels are displayed inside the segments.

  - `"outside"`: Labels are displayed outside the segments.

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

- pie_rotation:

  (numeric) The number of degrees to rotate the pie chart by, clockwise.
  The default is `0`.\

- centre_radius:

  (numeric, integer) The radius of the centre circle. Used to make a
  "donut" plot. Should be a value between 0 and

  1.  The default is `0`.\

- centre_label:

  (NULL, character) The text to display in the centre of the pie chart.
  Mostly used with donut plots where `centre_radius` is greater than 0.
  The default is `NULL`.

- count_na:

  (logical) Include the number of missing values in the pie chart. The
  default is `FALSE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` annotation_pie_chart ` object. This object has no `output` slots.
See [`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in
the `struct` package to plot this chart object.

## Details

This object makes use of functionality from the following packages:

- `ggplot2`

## Inheritance

A `annotation_pie_chart` object inherits the following `struct`
classes:\
\
`[annotation_pie_chart]` -\> `[chart]` -\> `[struct_class]`

## References

Wickham H (2016). *ggplot2: Elegant Graphics for Data Analysis*.
Springer-Verlag New York. ISBN 978-3-319-24277-4.
<https://ggplot2.tidyverse.org>.

## Examples

``` r
M <- annotation_pie_chart(
        factor_name = "V1",
        label_location = "inside",
        label_rotation = FALSE,
        legend = FALSE,
        pie_rotation = 0,
        label_type = "percent",
        centre_radius = 0,
        centre_label = NULL,
        count_na = FALSE)
```
