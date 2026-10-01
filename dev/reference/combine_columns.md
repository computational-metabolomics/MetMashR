# Combine columns

A wrapper for [`paste()`](https://rdrr.io/r/base/paste.html) and
[`interaction()`](https://rdrr.io/r/base/interaction.html). Combines the
values in multiple columns row-wise.

## Usage

``` r
combine_columns(
  column_names,
  separator = "_",
  prefix = NULL,
  suffix = NULL,
  output_column = "combined",
  clean = TRUE,
  ...
)
```

## Arguments

- column_names:

  (character) The column name(s) in the annotation_source to combine.

- separator:

  (character) A string placed in between the two being joined. The
  default is `"_"`.

- prefix:

  (character, NULL) A string placed at the start of the combined
  strings. The default is `NULL`.

- suffix:

  (character, NULL) A string placed at the end of the combined strings.
  The default is `NULL`.

- output_column:

  (character) The name of a column to store the combined values in. The
  default is `"combined"`.

- clean:

  (logical) Clean old columns. Allowed values are limited to the
  following:

  - `"TRUE"`: The named columns are removed after being combined.

  - `"FALSE"`: The named columns are retained after being combined.

  The default is `TRUE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `combine_columns` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The annotation_source after combining the columns. |

## Inheritance

A `combine_columns` object inherits the following `struct` classes:\
\
`[combine_columns]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- combine_columns(
        column_names = "V1",
        separator = "_",
        output_column = "combined",
        clean = FALSE,
        prefix = NULL,
        suffix = NULL)
```
