# Add columns

A wrapper around
[`dplyr::left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html).
Adds columns to an annotation table by performing a left-join with an
input data.frame (annotations on the left of the join).

## Usage

``` r
add_columns(new_columns, by, ...)
```

## Arguments

- new_columns:

  (data.frame, annotation_database) A data.frame to be left-joined to
  the annotation table. Can also be an annotation_database.

- by:

  (character) A (named) character vector of column names to join by e.g.
  `c("A" = "B")` (see
  [`dplyr::left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  for details).

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `add_columns` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The updated annotations as an `annotation_source` object. |

## Details

This object makes use of functionality from the following packages:

- `dplyr`

## Inheritance

A `add_columns` object inherits the following `struct` classes:\
\
`[add_columns]` -\> `[model]` -\> `[struct_class]`

## References

Wickham H, François R, Henry L, Müller K, Vaughan D (2026). *dplyr: A
Grammar of Data Manipulation*. doi:10.32614/CRAN.package.dplyr
<https://doi.org/10.32614/CRAN.package.dplyr>. R package version 1.2.1,
<https://CRAN.R-project.org/package=dplyr>.

## See also

[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)

## Examples

``` r
M <- add_columns(
        new_columns = data.frame(),
        by = "id")
```
