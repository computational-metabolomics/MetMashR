# rdata database

A data.frame stored as an RData file.

## Usage

``` r
rdata_database(source = character(0), variable_name, ...)
```

## Arguments

- source:

  (ANY) The source of annotation data. The default is `character(0)`.

- variable_name:

  (character, function) The name of the data.frame in the imported
  workspace to use as the data.frame for this source. A function can be
  provided to e.g. extract a data.frame from a list in the imported
  environment.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` rdata_database ` object. This object has no `output` slots.

## Inheritance

A `rdata_database` object inherits the following `struct` classes:\
\
`[rdata_database]` -\> `[annotation_database]` -\> `[annotation_source]`
-\> `[struct_class]`

## See also

Other annotation databases:
[`AnnotationDb_database()`](https://computational-metabolomics.github.io/MetMashR/reference/AnnotationDb_database.md),
[`GO_database()`](https://computational-metabolomics.github.io/MetMashR/reference/GO_database.md),
[`annotation_database()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_database.md),
[`annotation_source()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_source.md),
[`excel_database()`](https://computational-metabolomics.github.io/MetMashR/reference/excel_database.md),
[`rds_cache()`](https://computational-metabolomics.github.io/MetMashR/reference/rds_cache.md),
[`rds_database()`](https://computational-metabolomics.github.io/MetMashR/reference/rds_database.md)

## Examples

``` r
M <- rdata_database(
        variable_name = "a data frame",
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
