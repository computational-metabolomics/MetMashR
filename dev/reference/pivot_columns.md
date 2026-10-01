# Pivot longer

Combine multiple groups of columns into a single group of columns with
group labels.

## Usage

``` r
pivot_columns(column_groups, group_labels, ...)
```

## Arguments

- column_groups:

  (list) A named list of columns to group together into a single group
  of columns. There should be the same number of columns in each group.

- group_labels:

  (list) A named list of columns and the label to use for all records in
  that column.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `pivot_columns` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The updated annotations as an `annotation_source` object. |

## Details

This object makes use of functionality from the following packages:

- `dplyr`

## Inheritance

A `pivot_columns` object inherits the following `struct` classes:\
\
`[pivot_columns]` -\> `[model]` -\> `[struct_class]`

## References

Wickham H, François R, Henry L, Müller K, Vaughan D (2026). *dplyr: A
Grammar of Data Manipulation*. doi:10.32614/CRAN.package.dplyr
<https://doi.org/10.32614/CRAN.package.dplyr>. R package version 1.2.1,
<https://CRAN.R-project.org/package=dplyr>.

## Examples

``` r
M <- pivot_columns(
        group_labels = list(),
        column_groups = list())
```
