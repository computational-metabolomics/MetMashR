# msPurity source

An annotation source for importing an annotation table from the format
created by the `msPurity` package.

## Usage

``` r
mspurity_source(
  source,
  tag = "msPurity",
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
  The default is `"msPurity"`.

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

A ` mspurity_source ` object. This object has no `output` slots.

## Details

This object makes use of functionality from the following packages:

- `msPurity`

## Inheritance

A `mspurity_source` object inherits the following `struct` classes:\
\
`[mspurity_source]` -\> `[lcms_table]` -\> `[annotation_table]` -\>
`[annotation_source]` -\> `[struct_class]`

## References

Lawson, Nigel T, Weber, M. RJ, Jones, R. M, Chetwynd, J. A, Blanco R,
Alejandro G, Guida D, Riccardo, Viant, R. M, Dunn, B W (2017).
"msPurity: Automated Evaluation of Precursor Ion Purity for Mass
Spectrometry-Based Fragmentation in Metabolomics." *Analytical
Chemistry*, *89*, 2432-2439. doi:10.1021/acs.analchem.6b04358
<https://doi.org/10.1021/acs.analchem.6b04358>.

## See also

Other annotation sources:
[`annotation_database()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/annotation_database.md),
[`annotation_table()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/annotation_table.md),
[`cd_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/cd_source.md),
[`ls_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/ls_source.md),
[`mwb_study_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/mwb_study_source.md)

Other annotation tables:
[`annotation_table()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/annotation_table.md),
[`cd_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/cd_source.md),
[`ls_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/ls_source.md)

## Examples

``` r
M <- mspurity_source(
        mz_column = "mz",
        rt_column = "rt",
        id_column = "id",
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
