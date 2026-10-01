# LCMS table

An LCMS table extends
[`annotation_table()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/annotation_table.md)
to represent annotation data for an LCMS experiment. Columns
representing m/z and retention time are required for an `lcms_table`.

## Usage

``` r
lcms_table(
  data = NULL,
  tag = "",
  id_column = "id",
  mz_column = "mz",
  rt_column = "rt",
  ...
)
```

## Arguments

- data:

  (data.frame, NULL) A data.frame of annotation data. The default is
  `NULL`.

- tag:

  (character) A (short) character string that is used to represent this
  source e.g. in column names or source columns when used in a workflow.
  The default is `""`.

- id_column:

  (character) The column name of the annotation data.frame containing
  row identifers. If NULL This will be generated automatically. The
  default is `"id"`.

- mz_column:

  (character) The column name of the annotation data.frame containing
  m/z values. The default is `"mz"`.

- rt_column:

  (character) The column name of the annotation data.frame containing
  retention time values. The default is `"rt"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` lcms_table ` object. This object has no `output` slots.

## Inheritance

A `lcms_table` object inherits the following `struct` classes:\
\
`[lcms_table]` -\> `[annotation_table]` -\> `[annotation_source]` -\>
`[struct_class]`

## Examples

``` r
M <- lcms_table(
        mz_column = "mz",
        rt_column = "rt",
        id_column = "id",
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
