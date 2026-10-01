# Select columns from AnnotationDb database

A wrapper around `[annotationDbi::select()]` that can be used to import
columns from the database where the keys are provided by a column in the
annotation table.

## Usage

``` r
AnnotationDb_select(
  database,
  key_column,
  key_type,
  database_columns,
  drop_na = TRUE,
  ...
)
```

## Arguments

- database:

  (character) The name of the AnnotationDbi package/object to import.

- key_column:

  (character) The name of a column in the annotation table containing
  key values used to extract records from the AnnotationDbi database.

- key_type:

  (character) The name of a column in the AnnoationDb database searched
  for matches to the key values.

- database_columns:

  (character) The name of columns to import from the AnnoationDb
  database. Special case `.all` can be used to get all columns.

- drop_na:

  (logical) Drop NA. Allowed values are limited to the following:

  - `"TRUE"`: Remove rows where all columns of the returned database are
    NA.

  - `"FALSE"`: Keep rows where all columns of the returned database are
    NA.

  The default is `TRUE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `AnnotationDb_select` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The updated annotations as an `annotation_source` object. |

## Details

This object makes use of functionality from the following packages:

- `dplyr`

- `AnnotationDbi`

## Inheritance

A `AnnotationDb_select` object inherits the following `struct` classes:\
\
`[AnnotationDb_select]` -\> `[model]` -\> `[struct_class]`

## References

Wickham H, François R, Henry L, Müller K, Vaughan D (2026). *dplyr: A
Grammar of Data Manipulation*. doi:10.32614/CRAN.package.dplyr
<https://doi.org/10.32614/CRAN.package.dplyr>. R package version 1.2.1,
<https://CRAN.R-project.org/package=dplyr>.

Pagès H, Carlson M, Falcon S, Li N (2026). *AnnotationDbi: Manipulation
of SQLite-based annotations in Bioconductor*.
doi:10.18129/B9.bioc.AnnotationDbi
<https://doi.org/10.18129/B9.bioc.AnnotationDbi>. R package version
1.74.0, <https://bioconductor.org/packages/AnnotationDbi>.

## See also

[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)

[`AnnotationDbi::select()`](https://rdrr.io/pkg/AnnotationDbi/man/AnnotationDb-class.html)

## Examples

``` r
M <- AnnotationDb_select(
        database = "",
        key_column = "",
        key_type = "",
        database_columns = ".all",
        drop_na = FALSE)
```
