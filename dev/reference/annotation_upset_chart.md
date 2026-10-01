# Annotation UpSet chart

Display an UpSet chart of labels in the specified column of an
annotation_source.

## Usage

``` r
annotation_upset_chart(
  factor_name,
  group_column = NULL,
  order_intersect_by = "size",
  order_set_by = "name",
  nintersects = NULL,
  filter = NULL,
  relative_width = 0.3,
  relative_height = 3,
  top_bar_color = "grey30",
  top_bar_y_label = NULL,
  top_bar_show_numbers = TRUE,
  top_bar_numbers_size = 3,
  sets_bar_color = "grey30",
  sets_bar_show_numbers = FALSE,
  sets_bar_x_label = "Set Size",
  sets_bar_position = "left",
  intersection_matrix_color = "grey30",
  specific = TRUE,
  ...
)
```

## Arguments

- factor_name:

  (character) The name of the column(s) in the `annotation_source`(s) to
  generate an UpSet chart from.

- group_column:

  (character, NULL) The name of the column in the `annotation_source` to
  create groups from in the Venn diagram. This parameter is ignored if
  there are multiple input tables, as each table is considered to be a
  group. This parameter is also ignored if more than one `factor_name`
  is provided, as each column is considered a group. The default is
  `NULL`.

- order_intersect_by:

  (character) Order intersect by. Allowed values are limited to the
  following:

  - `"size"`: Intersections are sorted by size (largest first).

  - `"name"`: Intersections are sorted by name alphabetically.

  - `"none"`: Intersections are not sorted.

  The default is `"size"`.

- order_set_by:

  (character) Order set by. Allowed values are limited to the following:

  - `"size"`: Sets are sorted by size (largest first).

  - `"name"`: Sets are sorted by name alphabetically.

  - `"none"`: Sets are not sorted.

  The default is `"name"`.

- nintersects:

  (numeric, integer, NULL) The number of intersections to include in the
  plot. The default is `NULL`.

- filter:

  (function, NULL) A function or list of functions to filter
  intersections based on their properties. The function(s) should take
  region_data as input and return a logical vector indicating which
  intersections to keep. Use upset_min_size(), upset_min_groups(),
  upset_max_groups(), upset_intersections(), or create custom filter
  functions. The default is `NULL`.

- relative_width:

  (numeric) The relative width of the left panel in the upset plot. The
  default is `0.3`.\

- relative_height:

  (numeric) The relative height of the top panel in the upset plot. The
  default is `3`.\

- top_bar_color:

  (character) The color of the top bar chart showing intersection sizes.
  The default is `"grey30"`.

- top_bar_y_label:

  (character, NULL) The label for the Y-axis of the top bar chart. The
  default is `NULL`.

- top_bar_show_numbers:

  (logical) Whether to show numbers on the top bar chart. The default is
  `TRUE`.\

- top_bar_numbers_size:

  (numeric) The text size of numbers on the top bar chart. The default
  is `3`.\

- sets_bar_color:

  (character) The color of the sets bar chart. The default is
  `"grey30"`.

- sets_bar_show_numbers:

  (logical) Whether to show numbers on the sets bar chart. The default
  is `FALSE`.\

- sets_bar_x_label:

  (character) The label for the X-axis of the sets bar chart. The
  default is `"Set Size"`.

- sets_bar_position:

  (character) Sets bar position. Allowed values are limited to the
  following:

  - `"left"`: Position the sets bar chart on the left side.

  - `"right"`: Position the sets bar chart on the right side.

  The default is `"left"`.

- intersection_matrix_color:

  (character) The color of the intersection matrix dots and lines. The
  default is `"grey30"`.

- specific:

  (logical) Whether to include only specific items in subsets (TRUE) or
  all overlapping items (FALSE). The default is `TRUE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` annotation_upset_chart ` object. This object has no `output` slots.
See [`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in
the `struct` package to plot this chart object.

## Details

This object makes use of functionality from the following packages:

- `ggVennDiagram`

The plot object returned is of class 'aplot' which may not be compatible
with all plot combination functions. To combine with other ggplot
objects using cowplot or patchwork, use ggplotify::as.ggplot() to
convert the plot object:

    library(ggplotify)
    g <- chart_plot(C, data)
    g_ggplot <- as.ggplot(g)
    cowplot::plot_grid(g1, g_ggplot, nrow = 1)

## Note

The interface to this class has changed. Some parameters have been
renamed:

- 'width_ratio' -\> 'relative_width'

- 'xlabel' -\> 'top_bar_y_label'

- 'sort_intersections' -\> 'order_intersect_by'

- 'intersections' -\> 'nintersects'

- 'n_intersections' -\> 'nintersects'

- 'queries' -\> (removed)

- 'keep_empty_group' -\> (removed)

- 'sort_sets' -\> 'order_set_by'

Old parameter names will trigger deprecation warnings.

## Inheritance

A `annotation_upset_chart` object inherits the following `struct`
classes:\
\
`[annotation_upset_chart]` -\> `[chart]` -\> `[struct_class]`

## Filtering

Use the `filter` parameter to filter intersections based on their
properties:

    # Filter by minimum size
    C <- annotation_upset_chart(factor_name = "V1", filter = upset_min_size(5))

    # Filter by minimum number of groups
    C <- annotation_upset_chart(factor_name = "V1", filter = upset_min_groups(3))

    # Filter to show only specific combinations
    C <- annotation_upset_chart(factor_name = "V1",
                              filter = upset_intersections(c("A/B", "B/C")))

    # Custom filter function
    custom_filter <- function(region_data) {
      region_data$count >= 3 & grepl("A", region_data$name)
    }
    C <- annotation_upset_chart(factor_name = "V1", filter = custom_filter)

## References

Gao C, Dusa A (2026). *ggVennDiagram: A 'ggplot2' Implement of Venn
Diagram*. doi:10.32614/CRAN.package.ggVennDiagram
<https://doi.org/10.32614/CRAN.package.ggVennDiagram>. R package version
1.5.7, <https://CRAN.R-project.org/package=ggVennDiagram>.

## Examples

``` r
M <- annotation_upset_chart(
        factor_name = "V1",
        group_column = NULL,
        order_intersect_by = "size",
        order_set_by = "name",
        nintersects = NULL,
        filter = NULL,
        relative_width = 0.3,
        relative_height = 3,
        top_bar_color = "grey30",
        top_bar_y_label = NULL,
        top_bar_show_numbers = FALSE,
        top_bar_numbers_size = 3,
        sets_bar_color = "grey30",
        sets_bar_show_numbers = FALSE,
        sets_bar_x_label = "Set Size",
        sets_bar_position = "left",
        intersection_matrix_color = "grey30",
        specific = FALSE)
```
