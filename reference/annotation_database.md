# An annotation database

An `annotation_database` is an
[`annotation_source()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_source.md)
where the imported data.frame contains meta data for annotations. For
example it might be a table of molecular identifiers, associated
pathways etc.

## Usage

``` r
annotation_database(data = data.frame(), tag = "", ...)
```

## Arguments

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

A ` annotation_database ` object. This object has no `output` slots.

## Inheritance

A `annotation_database` object inherits the following `struct` classes:\
\
`[annotation_database]` -\> `[annotation_source]` -\> `[struct_class]`

## See also

Other annotation databases:
[`AnnotationDb_database()`](https://computational-metabolomics.github.io/MetMashR/reference/AnnotationDb_database.md),
[`GO_database()`](https://computational-metabolomics.github.io/MetMashR/reference/GO_database.md),
[`annotation_source()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_source.md),
[`excel_database()`](https://computational-metabolomics.github.io/MetMashR/reference/excel_database.md),
[`rdata_database()`](https://computational-metabolomics.github.io/MetMashR/reference/rdata_database.md),
[`rds_cache()`](https://computational-metabolomics.github.io/MetMashR/reference/rds_cache.md),
[`rds_database()`](https://computational-metabolomics.github.io/MetMashR/reference/rds_database.md)

Other annotation sources:
[`annotation_table()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_table.md),
[`cd_source()`](https://computational-metabolomics.github.io/MetMashR/reference/cd_source.md),
[`ls_source()`](https://computational-metabolomics.github.io/MetMashR/reference/ls_source.md),
[`mspurity_source()`](https://computational-metabolomics.github.io/MetMashR/reference/mspurity_source.md),
[`mwb_study_source()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_study_source.md)

## Examples

``` r
M <- annotation_database(
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
