# Metabolomics Workbench study source

Imports the reported metabolite list for a Metabolomics Workbench study,
using the `metabolomicsWorkbenchR` package. Only the identifiers a study
depositor chose to report are returned as-is (e.g. `metabolite_name`,
and where present `refmet_name`/`pubchem_id`/`other_id`) - no
translation or enrichment is performed here. To reproduce the scenario
of annotation software output that has names but no standardised
identifiers (the case MetMashR's mashing steps are designed for),
downstream workflow steps should use only the `metabolite_name` column
and treat any pre-existing `refmet_name`/`pubchem_id`/`other_id` values
as optional validation data, not as workflow input.

## Usage

``` r
mwb_study_source(source, tag = "MWB", analysis_id = NULL, data = NULL, ...)
```

## Arguments

- source:

  (character) A Metabolomics Workbench study identifier e.g. "ST001039".

- tag:

  (character) A (short) character string that is used to represent this
  source e.g. in column names or source columns when used in a workflow.
  The default is `"MWB"`.

- analysis_id:

  (character, NULL) Optionally restrict the imported metabolites to one
  or more specific analysis ids within the study (e.g. a single LC-MS
  assay). If `NULL` (the default), metabolites for all analyses in the
  study are returned. The default is `NULL`.

- data:

  (data.frame, NULL) A data.frame of annotation data. The default is
  `NULL`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` mwb_study_source ` object. This object has no `output` slots.

## Details

This object makes use of functionality from the following packages:

- `metabolomicsWorkbenchR`

## Inheritance

A `mwb_study_source` object inherits the following `struct` classes:\
\
`[mwb_study_source]` -\> `[annotation_source]` -\> `[struct_class]`

## References

Lloyd GR, Weber RJM (2026). *metabolomicsWorkbenchR: Metabolomics
Workbench in R*. R package version 1.22.0.

## See also

Other annotation sources:
[`annotation_database()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/annotation_database.md),
[`annotation_table()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/annotation_table.md),
[`cd_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/cd_source.md),
[`ls_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/ls_source.md),
[`mspurity_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/mspurity_source.md)

## Examples

``` r
M <- mwb_study_source(
        analysis_id = NULL,
        tag = character(0),
        data = data.frame(),
        source = character(0))
```
