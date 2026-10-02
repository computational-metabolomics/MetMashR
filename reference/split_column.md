# Split a column

A wrapper for [`strsplit`](https://rdrr.io/r/base/strsplit.html).
Divides a column into multiple columns by dividing the contents

## Usage

``` r
split_column(
  column_name,
  separator = "_",
  padding = NA,
  keep_indices = NULL,
  clean = TRUE,
  ...
)
```

## Arguments

- column_name:

  (character) The column name in the annotation_source split.

- separator:

  (character) A substring to split the column by. The default is `"_"`.

- padding:

  (character, logical) A character string used to represent missing and
  zero length strings after splitting. The default is `NA`.\

- keep_indices:

  (numeric, integer) The indices of columns to keep after splitting. If
  NULL then all columns are retained. The default is `NULL`.

- clean:

  (logical) Clean old columns. Allowed values are limited to the
  following:

  - `"TRUE"`: The named columns are removed after being split.

  - `"FALSE"`: The named columns are retained after being split.

  The default is `TRUE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `split_column` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The annotation_source after splitting the column. |

## Inheritance

A `split_column` object inherits the following `struct` classes:\
\
`[split_column]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- split_column(
        column_name = "V1",
        separator = "_",
        clean = FALSE,
        padding = FALSE,
        keep_indices = numeric(0))
```
