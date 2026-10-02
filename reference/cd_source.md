# LCMS table

An LCMS table extends
[`annotation_table()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_table.md)
to represent annotation data for an LCMS experiment. Columns
representing m/z and retention time are required for an `lcms_table`.

## Usage

``` r
cd_source(
  source,
  sheets = c(1, 1),
  tag = "CD",
  mz_column = "mz",
  rt_column = "rt",
  id_column = "id",
  data = NULL,
  ...
)
```

## Arguments

- source:

  (character) The path to the Compound Discoverer Excel files to import.
  Both the compounds and isomers file should be included, in that order.

- sheets:

  (character, numeric, integer) The name or index of the sheets to read
  from the source file(s). A sheet should be provided for each input
  file. The default is `c(1, 1)`.

- tag:

  (character) A (short) character string that is used to represent this
  source e.g. in column names or source columns when used in a workflow.
  The default is `"CD"`.

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

A ` cd_source ` object. This object has no `output` slots.

## Inheritance

A `cd_source` object inherits the following `struct` classes:\
\
`[cd_source]` -\> `[lcms_table]` -\> `[annotation_table]` -\>
`[annotation_source]` -\> `[struct_class]`

## See also

Other annotation sources:
[`annotation_database()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_database.md),
[`annotation_table()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_table.md),
[`ls_source()`](https://computational-metabolomics.github.io/MetMashR/reference/ls_source.md),
[`mspurity_source()`](https://computational-metabolomics.github.io/MetMashR/reference/mspurity_source.md),
[`mwb_study_source()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_study_source.md)

Other annotation tables:
[`annotation_table()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_table.md),
[`ls_source()`](https://computational-metabolomics.github.io/MetMashR/reference/ls_source.md),
[`mspurity_source()`](https://computational-metabolomics.github.io/MetMashR/reference/mspurity_source.md)

## Examples

``` r
M <- cd_source(
        sheets = c(1, 1),
        mz_column = "mz",
        rt_column = "rt",
        id_column = "id",
        tag = character(0),
        data = data.frame(),
        source = character(0))
```
