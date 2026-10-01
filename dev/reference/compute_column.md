# Compute a column

Compute values for a new column based on an input column.

## Usage

``` r
compute_column(input_columns, output_column, fcn, ...)
```

## Arguments

- input_columns:

  (character) The name of a column in the input table used to compute a
  new column.

- output_column:

  (character) The name of the newply computed column.

- fcn:

  (function) The function used to compute the values for the new column.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `compute_column` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The updated annotations as an `annotation_source` object. |

## Details

This object makes use of functionality from the following packages:

- `dplyr`

## Inheritance

A `compute_column` object inherits the following `struct` classes:\
\
`[compute_column]` -\> `[model]` -\> `[struct_class]`

## References

Wickham H, François R, Henry L, Müller K, Vaughan D (2026). *dplyr: A
Grammar of Data Manipulation*. doi:10.32614/CRAN.package.dplyr
<https://doi.org/10.32614/CRAN.package.dplyr>. R package version 1.2.1,
<https://CRAN.R-project.org/package=dplyr>.

## Examples

``` r
M <- compute_column(
        input_columns = character(0),
        output_column = character(0),
        fcn = function(){})
```
