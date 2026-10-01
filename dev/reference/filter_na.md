# Filter by missing values

Filters annotations where the named column is NA

## Usage

``` r
filter_na(column_name, mode = "exclude", ...)
```

## Arguments

- column_name:

  (character) The column name to use for filtering.

- mode:

  (character) Filter mode. Allowed values are limited to the following:

  - `"include"`: Rows with NA are kept and all others removed.

  - `"exclude"`: Rows with NA are excluded and all other kept.

  The default is `"exclude"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `filter_na` object with the following `output` slots:

|  |  |
|----|----|
| `filtered` | (annotation_source) Annotation_source after filtering. |
| `flags` | (data.frame) A list of flags indicating which annotations were removed. |

## Inheritance

A `filter_na` object inherits the following `struct` classes:\
\
`[filter_na]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- filter_na(
        column_name = "V1",
        mode = "exclude")
```
