# <u>Met</u>abolite <u>Mash</u>ing in <u>R</u> (MetMashR)

<!-- badges: start -->
[![BioC version](https://img.shields.io/badge/dynamic/yaml?url=https%3A%2F%2Fbioconductor.org%2Fconfig.yaml&query=%24.release_version&label=Bioconductor)](https://bioconductor.org/packages/MetMashR)
[![BioC status](https://bioconductor.org/shields/build/release/bioc/MetMashR.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/MetMashR)
<!-- badges: end -->

`MetMashR` is an R package that can be used to import, clean, filter,
prioritise, combine and otherwise "mash" together metabolite annotations from
multiple sources.

## Summary

Metabolomics measures the small molecules (metabolites) in a sample, such as
blood, urine or water. Instruments like mass spectrometers detect thousands of
signals, and software tools try to work out which molecule each signal belongs
to. This is called annotation. Different tools often disagree, name the same
molecule in different ways, or report several possible matches for one signal.

`MetMashR` helps researchers bring these annotations together. It reads the
output of different annotation tools into a common format, cleans and
standardises molecule names, looks up extra information in public databases
such as PubChem, ChEBI, KEGG and LIPID MAPS, and filters and combines the
results. Each of these jobs is a separate building block, and blocks are
chained into a workflow. The workflow can be shared and re-run, so the same
steps are applied in the same way every time.

## Links

- Documentation (release): <https://computational-metabolomics.github.io/MetMashR/>
- Documentation (devel): <https://computational-metabolomics.github.io/MetMashR/dev/>
- Bioconductor (release): <https://bioconductor.org/packages/release/bioc/html/MetMashR.html>
- Bioconductor (devel): <https://bioconductor.org/packages/devel/bioc/html/MetMashR.html>
- Source code: <https://github.com/computational-metabolomics/MetMashR>
- Bug reports: <https://github.com/computational-metabolomics/MetMashR/issues>

## Vignettes and case studies

- [Using MetMashR](https://computational-metabolomics.github.io/MetMashR/articles/using_MetMashR.html):
  an introduction to annotation sources, workflow steps, REST API lookups,
  dictionaries and combining records.
- [Extending MetMashR](https://computational-metabolomics.github.io/MetMashR/articles/Extending_MetMashR.html):
  how to create your own annotation sources and workflow steps.
- [Annotation of mixtures of standards](https://computational-metabolomics.github.io/MetMashR/articles/annotate_mixtures.html):
  a case study combining Compound Discoverer and LipidSearch annotations of
  mixtures of chemical standards, and assessing them against the known contents
  of the mixtures.
- [Exploring the MTox700+ library](https://computational-metabolomics.github.io/MetMashR/articles/exploring_mtox.html):
  a case study combining the MTox700+ database of toxicologically relevant
  metabolites with other databases (e.g. PathBank) to explore its chemical and
  biological coverage.
- [Case study: reimplementing the Metabolites Merging Strategy](https://computational-metabolomics.github.io/MetMashR/articles/mms_case_study.html):
  a case study reproducing a published annotation-merging strategy as a
  `MetMashR` workflow.

## Installation

To install the release version from Bioconductor:

```r
if (!require("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("MetMashR")
```

To install the development version from GitHub:

```r
if (!require("remotes", quietly = TRUE)) {
    install.packages("remotes")
}

remotes::install_github("computational-metabolomics/MetMashR")
```

## Example workflow

This example uses LipidSearch annotations that are included with the package.
The workflow imports the annotations, keeps only the higher-confidence ones
(grades A and B), then combines duplicate annotations of the same lipid into a
single record.

```r
library(MetMashR)

# annotation source: a LipidSearch output file
AT <- ls_source(
    source = system.file(
        "extdata/MTox/LS/MTox_2023_HILIC_POS.txt",
        package = "MetMashR"
    )
)

# workflow
WF <-
    # step 1: import the annotations
    import_source() +
    # step 2: keep annotations with grade A or B
    filter_labels(
        column_name = "Grade",
        labels = c("A", "B"),
        mode = "include"
    ) +
    # step 3: combine records for the same lipid, counting how many there were
    combine_records(
        group_by = "LipidName",
        default_fcn = fuse_unique(separator = "; "),
        fcns = list(count = count_records())
    )

# apply the workflow to the annotation source
WF <- model_apply(WF, AT)

# the processed annotations
predicted(WF)
head(predicted(WF)$data[, c("LipidName", "Grade", "count")])

# the annotations after any individual step, e.g. after filtering
predicted(WF[2])
```

The [Using MetMashR](https://computational-metabolomics.github.io/MetMashR/articles/using_MetMashR.html)
vignette describes these and many other workflow steps in detail.
