# An annotation table

An `annotation_table` is an
[`annotation_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/annotation_source.md)
where the imported data.frame contains measured experimental data. An
`id_column` of values is required to uniquely indentify each record
(row) in the table (NB these are NOT molecule identifiers, which may be
be present in multiple records).

## Usage

``` r
annotation_table(data = data.frame(), tag = "", id_column = NULL, ...)
```

## Arguments

- data:

  (data.frame, NULL) A data.frame of annotation data. The default is
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html).

- tag:

  (character) A (short) character string that is used to represent this
  source e.g. in column names or source columns when used in a workflow.
  The default is `""`.

- id_column:

  (character) The column name of the annotation data.frame containing
  row identifers. If NULL This will be generated automatically. The
  default is `NULL`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` annotation_table ` object. This object has no `output` slots.

## Inheritance

A `annotation_table` object inherits the following `struct` classes:\
\
`[annotation_table]` -\> `[annotation_source]` -\> `[struct_class]`

## See also

Other annotation tables:
[`cd_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/cd_source.md),
[`ls_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/ls_source.md),
[`mspurity_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/mspurity_source.md)

Other annotation sources:
[`annotation_database()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/annotation_database.md),
[`cd_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/cd_source.md),
[`ls_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/ls_source.md),
[`mspurity_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/mspurity_source.md),
[`mwb_study_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/mwb_study_source.md)

## Examples

``` r
M <- annotation_table(
        id_column = "id",
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
