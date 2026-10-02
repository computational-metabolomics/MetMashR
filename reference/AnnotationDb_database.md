# AnnotationDb database

Retrieve a table from an AnnotationDb package.

## Usage

``` r
AnnotationDb_database(source, table, ...)
```

## Arguments

- source:

  (character) The name of an AnnotationDb package to import the
  specified table from. Note the package should already be installed.

- table:

  (character) The name of a table to import from the specified source
  AnnotationDb package.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` AnnotationDb_database ` object. This object has no `output` slots.

## Details

This object makes use of functionality from the following packages:

- `AnnotationDbi`

## Inheritance

A `AnnotationDb_database` object inherits the following `struct`
classes:\
\
`[AnnotationDb_database]` -\> `[annotation_database]` -\>
`[annotation_source]` -\> `[struct_class]`

## References

Pagès H, Carlson M, Falcon S, Li N (2026). *AnnotationDbi: Manipulation
of SQLite-based annotations in Bioconductor*.
doi:10.18129/B9.bioc.AnnotationDbi
<https://doi.org/10.18129/B9.bioc.AnnotationDbi>. R package version
1.74.0, <https://bioconductor.org/packages/AnnotationDbi>.

## See also

[AnnotationDbi::AnnotationDb](https://rdrr.io/pkg/AnnotationDbi/man/AnnotationDb-class.html)

Other annotation databases:
[`GO_database()`](https://computational-metabolomics.github.io/MetMashR/reference/GO_database.md),
[`annotation_database()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_database.md),
[`annotation_source()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_source.md),
[`excel_database()`](https://computational-metabolomics.github.io/MetMashR/reference/excel_database.md),
[`rdata_database()`](https://computational-metabolomics.github.io/MetMashR/reference/rdata_database.md),
[`rds_cache()`](https://computational-metabolomics.github.io/MetMashR/reference/rds_cache.md),
[`rds_database()`](https://computational-metabolomics.github.io/MetMashR/reference/rds_database.md)

## Examples

``` r
M <- AnnotationDb_database(
        table = character(0),
        tag = character(0),
        data = data.frame(),
        source = character(0))
```
