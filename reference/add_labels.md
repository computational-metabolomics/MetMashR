# Add column of labels

Adds new columns with the specified labels for each record.

## Usage

``` r
add_labels(labels, replace = FALSE, ...)
```

## Arguments

- labels:

  (list) A named list of columns and the label to use for all records in
  that column.

- replace:

  (logical) Replace columns. Allowed values are limited to the
  following:

  - `"TRUE"`: If present, the new columns will replace existing columns
    in the source data.frame.

  - `"FALSE"`: An error will be thrown if the new columns are already in
    the source data.frame.

  The default is `FALSE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `add_labels` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The updated annotations as an `annotation_source` object. |

## Details

This object makes use of functionality from the following packages:

- `dplyr`

## Inheritance

A `add_labels` object inherits the following `struct` classes:\
\
`[add_labels]` -\> `[model]` -\> `[struct_class]`

## References

Wickham H, François R, Henry L, Müller K, Vaughan D (2026). *dplyr: A
Grammar of Data Manipulation*. doi:10.32614/CRAN.package.dplyr
<https://doi.org/10.32614/CRAN.package.dplyr>. R package version 1.2.1,
<https://CRAN.R-project.org/package=dplyr>.

## Examples

``` r
M <- add_labels(
        labels = list(),
        replace = FALSE)
```
