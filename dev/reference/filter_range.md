# Filter by range

Removes annotations where the names column is greater than an upper
limit or less than a lower limit.

## Usage

``` r
filter_range(
  column_name,
  upper_limit = Inf,
  lower_limit = -Inf,
  equal_to = TRUE,
  ...
)
```

## Arguments

- column_name:

  (character) The column name to filter.

- upper_limit:

  (numeric, integer, function) The upper limit used for filtering. Can
  be a value, or a function that computes a value (e.g. mean). The
  default is `Inf`.\

- lower_limit:

  (numeric, integer, function) The lower limit used for filtering. Can
  be a value, or a function that computes a value (e.g. mean). The
  default is `-Inf`.

- equal_to:

  (logical) Equal to limits. Allowed values are limited to the
  following:

  - `"TRUE"`: Greater/less than or equal to the limits are excluded.

  - `"FALSE"`: Greater/less than the limits are excluded.

  The default is `TRUE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `filter_range` object with the following `output` slots:

|  |  |
|----|----|
| `filtered` | (annotation_source) Annotation_source after filtering. |
| `flags` | (data.frame) A list of flags indicating which annotations were removed. |

## Inheritance

A `filter_range` object inherits the following `struct` classes:\
\
`[filter_range]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- filter_range(
        column_name = "V1",
        upper_limit = Inf,
        lower_limit = -Inf,
        equal_to = FALSE)
```
