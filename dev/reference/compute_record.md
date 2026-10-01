# Compute a value for a record

Compute values for a record based on other values in a record

## Usage

``` r
compute_record(fcn, ...)
```

## Arguments

- fcn:

  (function) The function used to compute the values for the record.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `compute_record` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The updated annotations as an `annotation_source` object. |

## Details

This object makes use of functionality from the following packages:

- `dplyr`

## Inheritance

A `compute_record` object inherits the following `struct` classes:\
\
`[compute_record]` -\> `[model]` -\> `[struct_class]`

## References

Wickham H, François R, Henry L, Müller K, Vaughan D (2026). *dplyr: A
Grammar of Data Manipulation*. doi:10.32614/CRAN.package.dplyr
<https://doi.org/10.32614/CRAN.package.dplyr>. R package version 1.2.1,
<https://CRAN.R-project.org/package=dplyr>.

## Examples

``` r
M <- compute_record(
        fcn = function(){})
```
