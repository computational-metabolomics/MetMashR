# Filter rows

A wrapper around
[`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).
Select rows from an annotation table using tidy grammar.

## Usage

``` r
filter_records(where = wherever(A > 0), ...)
```

## Arguments

- where:

  (quosures) A list of
  [`rlang::quosure`](https://rlang.r-lib.org/reference/quosure-tools.html)
  for evaluation e.g. A\>10 willselect all rows where the values in
  column A are greater than10. A helper function
  [`wherever`](https://computational-metabolomics.github.io/MetMashR/reference/wherever.md)
  is provided to generatea suitable list of quosures. The default is
  `wherever(A > 0)`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `filter_records` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The updated annotations as an `annotation_source` object. |

## Details

This object makes use of functionality from the following packages:

- `dplyr`

- `rlang`

## Inheritance

A `filter_records` object inherits the following `struct` classes:\
\
`[filter_records]` -\> `[model]` -\> `[struct_class]`

## References

Wickham H, François R, Henry L, Müller K, Vaughan D (2026). *dplyr: A
Grammar of Data Manipulation*. doi:10.32614/CRAN.package.dplyr
<https://doi.org/10.32614/CRAN.package.dplyr>. R package version 1.2.1,
<https://CRAN.R-project.org/package=dplyr>.

Henry L, Wickham H (2026). *rlang: Functions for Base Types and Core R
and 'Tidyverse' Features*. doi:10.32614/CRAN.package.rlang
<https://doi.org/10.32614/CRAN.package.rlang>. R package version 1.3.0,
<https://CRAN.R-project.org/package=rlang>.

## See also

[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)

[`wherever()`](https://computational-metabolomics.github.io/MetMashR/reference/wherever.md)

## Examples

``` r
M <- filter_records(
        where = wherever(A>10))
```
