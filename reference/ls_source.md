# LCMS table

An LCMS table extends
[`annotation_table()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_table.md)
to represent annotation data for an LCMS experiment. Columns
representing m/z and retention time are required for an `lcms_table`.

## Usage

``` r
ls_source(
  source,
  tag = "LS",
  mz_column = "mz",
  rt_column = "rt",
  id_column = "id",
  data = NULL,
  ...
)
```

## Arguments

- source:

  (ANY) The source of annotation data.

- tag:

  (character) A (short) character string that is used to represent this
  source e.g. in column names or source columns when used in a workflow.
  The default is `"LS"`.

- mz_column:

  (character) The column name of the annotation data.frame containing
  m/z values. The default is `"mz"`.

- rt_column:

  (character) The column name of the annotation data.frame containing
  retention time values. The default is `"rt"`.

- id_column:

  (character) The column name of the annotation data.frame containing
  row identifers. If NULL This will be generated automatically. The
  default is `"id"`.

- data:

  (data.frame, NULL) A data.frame of annotation data. The default is
  `NULL`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` ls_source ` object. This object has no `output` slots.

## Inheritance

A `ls_source` object inherits the following `struct` classes:\
\
`[ls_source]` -\> `[lcms_table]` -\> `[annotation_table]` -\>
`[annotation_source]` -\> `[struct_class]`

## See also

Other annotation sources:
[`annotation_database()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_database.md),
[`annotation_table()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_table.md),
[`cd_source()`](https://computational-metabolomics.github.io/MetMashR/reference/cd_source.md),
[`mspurity_source()`](https://computational-metabolomics.github.io/MetMashR/reference/mspurity_source.md),
[`mwb_study_source()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_study_source.md)

Other annotation tables:
[`annotation_table()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_table.md),
[`cd_source()`](https://computational-metabolomics.github.io/MetMashR/reference/cd_source.md),
[`mspurity_source()`](https://computational-metabolomics.github.io/MetMashR/reference/mspurity_source.md)

## Examples

``` r
M <- ls_source(
        mz_column = "mz",
        rt_column = "rt",
        id_column = "id",
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
