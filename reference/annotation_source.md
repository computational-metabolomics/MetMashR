# An annotation source

A base class defining an annotation source. This object is extended by
MetmashR to define other objects.

## Usage

``` r
annotation_source(source = character(0), data = data.frame(), tag = "", ...)
```

## Arguments

- source:

  (ANY) The source of annotation data. The default is `character(0)`.

- data:

  (data.frame, NULL) A data.frame of annotation data. The default is
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html).

- tag:

  (character) A (short) character string that is used to represent this
  source e.g. in column names or source columns when used in a workflow.
  The default is `""`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` annotation_source ` object. This object has no `output` slots.

## Inheritance

A `annotation_source` object inherits the following `struct` classes:\
\
`[annotation_source]` -\> `[struct_class]`

## See also

Other annotation databases:
[`AnnotationDb_database()`](https://computational-metabolomics.github.io/MetMashR/reference/AnnotationDb_database.md),
[`GO_database()`](https://computational-metabolomics.github.io/MetMashR/reference/GO_database.md),
[`annotation_database()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_database.md),
[`excel_database()`](https://computational-metabolomics.github.io/MetMashR/reference/excel_database.md),
[`rdata_database()`](https://computational-metabolomics.github.io/MetMashR/reference/rdata_database.md),
[`rds_cache()`](https://computational-metabolomics.github.io/MetMashR/reference/rds_cache.md),
[`rds_database()`](https://computational-metabolomics.github.io/MetMashR/reference/rds_database.md)

## Examples

``` r
M <- annotation_source(
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
