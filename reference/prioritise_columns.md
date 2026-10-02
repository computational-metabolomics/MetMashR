# Combine several columns into a single column.

Several columns are merged into a single column. If multiple columns
contain overlapping values then priority can be given columns earlier in
the list.

## Usage

``` r
prioritise_columns(
  column_names,
  output_name,
  source_name,
  source_tags = column_names,
  clean = TRUE,
  ...
)
```

## Arguments

- column_names:

  (character) The name(s) of column(s) to be combined.

- output_name:

  (character) The name of the new column.

- source_name:

  (character) The column name used to indicate the where the merged
  values originated.

- source_tags:

  (character) The tags used to identify the source of each item in the
  new column. A tag should be provided for each column_name. By default
  the column name is used.

- clean:

  (logical) Clean old columns. Allowed values are limited to the
  following:

  - `"TRUE"`: The named columns are removed after being combined.

  - `"FALSE"`: The named columns are retained after being combined.

  The default is `TRUE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `prioritise_columns` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The input annotation source with the newly generated column. |

## Inheritance

A `prioritise_columns` object inherits the following `struct` classes:\
\
`[prioritise_columns]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- prioritise_columns(
        column_names = "V1",
        output_name = "",
        clean = FALSE,
        source_name = "source_name",
        source_tags = "x")
```
