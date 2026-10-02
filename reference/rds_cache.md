# rds cache

A data.frame stored as an RDS file. Intended to be used with `rest_api`
objects as mechanism for caching search results. The data.frame for an
`rds_cache` object must have a column named ".search".

## Usage

``` r
rds_cache(
  source = character(0),
  data = data.frame(.search = character(0)),
  ...
)
```

## Arguments

- source:

  (ANY) The source of annotation data. The default is `character(0)`.

- data:

  (data.frame, NULL) A data.frame of annotation data. The default is
  `data.frame(.search = character(0))`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` rds_cache ` object. This object has no `output` slots.

## Inheritance

A `rds_cache` object inherits the following `struct` classes:\
\
`[rds_cache]` -\> `[rds_database]` -\> `[annotation_database]` -\>
`[annotation_source]` -\> `[struct_class]`

## See also

Other annotation databases:
[`AnnotationDb_database()`](https://computational-metabolomics.github.io/MetMashR/reference/AnnotationDb_database.md),
[`GO_database()`](https://computational-metabolomics.github.io/MetMashR/reference/GO_database.md),
[`annotation_database()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_database.md),
[`annotation_source()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_source.md),
[`excel_database()`](https://computational-metabolomics.github.io/MetMashR/reference/excel_database.md),
[`rdata_database()`](https://computational-metabolomics.github.io/MetMashR/reference/rdata_database.md),
[`rds_database()`](https://computational-metabolomics.github.io/MetMashR/reference/rds_database.md)

## Examples

``` r
M <- rds_cache(
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
