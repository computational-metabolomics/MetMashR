# Combine annotation records (rows)

Combine annotation records (rows) based on a key. All records with the
same key will be combined. A number of helper functions are provided for
common approaches to merging records.

## Usage

``` r
combine_records(
  group_by,
  default_fcn = fuse(separator = " || "),
  fcns = list(),
  ...
)
```

## Arguments

- group_by:

  (character) The column used as the key for grouping records.

- default_fcn:

  (function) The default function to use for summarising columns when
  combining records and a specific function has not been provided in
  fcns. The default is `fuse(separator = " || ")`.

- fcns:

  (list) A named list of functions to use for summarising named columns
  when combining records. Names should correspond to the columns in the
  annotation table. The default is
  [`list()`](https://rdrr.io/r/base/list.html).

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `combine_records` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The input annotation source with the newly generated column. |

## Details

This object makes use of functionality from the following packages:

- `dplyr`

## Inheritance

A `combine_records` object inherits the following `struct` classes:\
\
`[combine_records]` -\> `[model]` -\> `[struct_class]`

## References

Wickham H, François R, Henry L, Müller K, Vaughan D (2026). *dplyr: A
Grammar of Data Manipulation*. doi:10.32614/CRAN.package.dplyr
<https://doi.org/10.32614/CRAN.package.dplyr>. R package version 1.2.1,
<https://CRAN.R-project.org/package=dplyr>.

Lloyd GR, Jankevics A, Weber RJM (2020). "struct: an
R/Bioconductor-based framework for standardized metabolomics data
analysis and beyond." *Bioinformatics*, *36*(22-23), 5551-5552.

## Examples

``` r
M <- combine_records(
        fcns = list(),
        group_by = character(0),
        default_fcn = function(){})
```
