# id counts

Adds the number of times an identical identifier is present to each
record.

## Usage

``` r
id_counts(id_column, count_column = "id_counts", count_na = TRUE, ...)
```

## Arguments

- id_column:

  (character) Column name of the variable ids in variable_meta.

- count_column:

  (character) The name of the new column to store the counts in. The
  default is `"id_counts"`.

- count_na:

  (logical) Count NA. Allowed values are limited to the following:

  - `"TRUE"`: Report number of NA.

  - `"FALSE"`: Do not report number of NA.

  The default is `TRUE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `id_counts` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The input annotation source with the newly generated column. |

## Inheritance

A `id_counts` object inherits the following `struct` classes:\
\
`[id_counts]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- id_counts(
        id_column = character(0),
        count_column = character(0),
        count_na = FALSE)
```
