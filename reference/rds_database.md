# rds database

A data.frame stored as an RDS file.

## Usage

``` r
rds_database(source = character(0), ...)
```

## Arguments

- source:

  (ANY) The source of annotation data. The default is `character(0)`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` rds_database ` object. This object has no `output` slots.

## Inheritance

A `rds_database` object inherits the following `struct` classes:\
\
`[rds_database]` -\> `[annotation_database]` -\> `[annotation_source]`
-\> `[struct_class]`

## See also

Other annotation databases:
[`AnnotationDb_database()`](https://computational-metabolomics.github.io/MetMashR/reference/AnnotationDb_database.md),
[`GO_database()`](https://computational-metabolomics.github.io/MetMashR/reference/GO_database.md),
[`annotation_database()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_database.md),
[`annotation_source()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_source.md),
[`excel_database()`](https://computational-metabolomics.github.io/MetMashR/reference/excel_database.md),
[`rdata_database()`](https://computational-metabolomics.github.io/MetMashR/reference/rdata_database.md),
[`rds_cache()`](https://computational-metabolomics.github.io/MetMashR/reference/rds_cache.md)

## Examples

``` r
M <- rds_database(
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
