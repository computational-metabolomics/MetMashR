# Select columns

A wrapper around
[`dplyr::rename`](https://dplyr.tidyverse.org/reference/rename.html).
Rename columns from an annotation table using tidy grammar.

## Usage

``` r
rename_columns(expression, ...)
```

## Arguments

- expression:

  (call) A valid rlang::expr for tidy evaluation e.g.
  `expression = all_of(c("foo"="bar"))` will rename the column named
  "bar" and "foo". .

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `rename_columns` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The updated annotations as an `annotation_source` object. |

## Details

This object makes use of functionality from the following packages:

- `tidyselect`

- `rlang`

## Inheritance

A `rename_columns` object inherits the following `struct` classes:\
\
`[rename_columns]` -\> `[model]` -\> `[struct_class]`

## References

Henry L, Wickham H (2024). *tidyselect: Select from a Set of Strings*.
doi:10.32614/CRAN.package.tidyselect
<https://doi.org/10.32614/CRAN.package.tidyselect>. R package version
1.2.1, <https://CRAN.R-project.org/package=tidyselect>.

Henry L, Wickham H (2026). *rlang: Functions for Base Types and Core R
and 'Tidyverse' Features*. doi:10.32614/CRAN.package.rlang
<https://doi.org/10.32614/CRAN.package.rlang>. R package version 1.3.0,
<https://CRAN.R-project.org/package=rlang>.

## Examples

``` r
M <- rename_columns(
        expression = call("example"))
```
