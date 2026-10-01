# Select columns

A wrapper around
[`tidyselect::eval_select`](https://tidyselect.r-lib.org/reference/eval_select.html).
Select columns from an annotation table using tidy grammar. This
imitates
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).

## Usage

``` r
select_columns(expression = everything(), ...)
```

## Arguments

- expression:

  (call) A valid rlang::expr for tidy evaluation via eval_select. e.g.
  `expression = all_of(c("foo","bar"))` will select columns named "foo"
  and "bar" from the annotation data.frame. . The default is
  `everything()`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `select_columns` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The updated annotations as an `annotation_source` object. |

## Details

This object makes use of functionality from the following packages:

- `tidyselect`

- `rlang`

## Inheritance

A `select_columns` object inherits the following `struct` classes:\
\
`[select_columns]` -\> `[model]` -\> `[struct_class]`

## References

Henry L, Wickham H (2024). *tidyselect: Select from a Set of Strings*.
doi:10.32614/CRAN.package.tidyselect
<https://doi.org/10.32614/CRAN.package.tidyselect>. R package version
1.2.1, <https://CRAN.R-project.org/package=tidyselect>.

Henry L, Wickham H (2026). *rlang: Functions for Base Types and Core R
and 'Tidyverse' Features*. doi:10.32614/CRAN.package.rlang
<https://doi.org/10.32614/CRAN.package.rlang>. R package version 1.3.0,
<https://CRAN.R-project.org/package=rlang>.

## See also

[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)

[`tidyselect::eval_select()`](https://tidyselect.r-lib.org/reference/eval_select.html)

## Examples

``` r
M <- select_columns(
        expression = call("example"))
```
