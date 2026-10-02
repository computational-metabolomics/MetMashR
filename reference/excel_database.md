# Excel database

A data.frame imported from the sheet of an excel file

## Usage

``` r
excel_database(
  source = character(0),
  sheet = 1,
  rowNames = FALSE,
  colNames = TRUE,
  startRow = 1,
  ...
)
```

## Arguments

- source:

  (ANY) The source of annotation data. The default is `character(0)`.

- sheet:

  (character) The name of the sheet to import. The default is `1`.\

- rowNames:

  (logical) If TRUE, first column of data will be used as row names. The
  default is `FALSE`.\

- colNames:

  (logical) If TRUE, first row of data will be used as column names. The
  default is `TRUE`.\

- startRow:

  (numeric, integer) First row to begin looking for data. Empty rows at
  the top of a file are always skipped, regardless of the value of
  startRow. The default is `1`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` excel_database ` object. This object has no `output` slots.

## Details

This object makes use of functionality from the following packages:

- `openxlsx`

## Inheritance

A `excel_database` object inherits the following `struct` classes:\
\
`[excel_database]` -\> `[annotation_database]` -\> `[annotation_source]`
-\> `[struct_class]`

## References

Schauberger P, Walker A (2026). *openxlsx: Read, Write and Edit xlsx
Files*. doi:10.32614/CRAN.package.openxlsx
<https://doi.org/10.32614/CRAN.package.openxlsx>. R package version
4.2.9, <https://CRAN.R-project.org/package=openxlsx>.

## See also

Other annotation databases:
[`AnnotationDb_database()`](https://computational-metabolomics.github.io/MetMashR/reference/AnnotationDb_database.md),
[`GO_database()`](https://computational-metabolomics.github.io/MetMashR/reference/GO_database.md),
[`annotation_database()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_database.md),
[`annotation_source()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_source.md),
[`rdata_database()`](https://computational-metabolomics.github.io/MetMashR/reference/rdata_database.md),
[`rds_cache()`](https://computational-metabolomics.github.io/MetMashR/reference/rds_cache.md),
[`rds_database()`](https://computational-metabolomics.github.io/MetMashR/reference/rds_database.md)

## Examples

``` r
M <- excel_database(
        sheet = character(0),
        rowNames = FALSE,
        colNames = FALSE,
        startRow = 1,
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
