# *Met*abolite *Mash*ing in *R* (MetMashR)

[![BioC
version](https://img.shields.io/badge/dynamic/yaml?url=https%3A%2F%2Fbioconductor.org%2Fconfig.yaml&query=%24.release_version&label=Bioconductor)](https://bioconductor.org/packages/MetMashR)
[![BioC
status](https://bioconductor.org/shields/build/release/bioc/MetMashR.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/MetMashR)
[![License:
GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)

`MetMashR` is an R/Bioconductor package for integrating, harmonising and
processing metabolite annotations from multiple sources using modular,
reproducible workflows.

## Summary

Metabolomics is the analysis of small molecules, or metabolites, in
biological and environmental samples (e.g. blood, urine and water).
Liquid chromatography–mass spectrometry (LC–MS) is widely used in
metabolomics and produces complex datasets from which thousands of LC–MS
features can be detected. Computational annotation tools attempt to
associate these features with candidate chemical compounds, but
different tools may produce conflicting annotations, use inconsistent
compound names or identifiers, or report multiple candidates for a
single feature.

`MetMashR` integrates and harmonises metabolite annotations from
multiple sources. It imports annotation outputs into a common structure
and provides modular tools for cleaning, standardising, filtering,
prioritising and combining annotations, as well as enriching them with
information from resources such as PubChem, ChEBI, KEGG and LIPID MAPS.
These steps can be combined into reproducible workflows that can be
reused and applied consistently across analyses. MetMashR was developed
primarily for LC–MS annotation workflows, but its modular framework can
also be extended to other analytical platforms.

## Documentation and resources

- Documentation:
  <https://computational-metabolomics.github.io/MetMashR/>

- Bioconductor

  - release:
    <https://bioconductor.org/packages/release/bioc/html/MetMashR.html>
  - devel:
    <https://bioconductor.org/packages/devel/bioc/html/MetMashR.html>

- GitHub

  - Source code:
    <https://github.com/computational-metabolomics/MetMashR>
  - Bug reports and feature requests:
    <https://github.com/computational-metabolomics/MetMashR/issues>

### Vignettes and case studies

- [Using
  MetMashR](https://computational-metabolomics.github.io/MetMashR/articles/using_MetMashR.html):
  an introduction to annotation sources, workflow steps, REST API
  lookups, dictionaries and combining records.
- [Extending
  MetMashR](https://computational-metabolomics.github.io/MetMashR/articles/Extending_MetMashR.html):
  how to create your own annotation sources and workflow steps.
- [Annotation of mixtures of
  standards](https://computational-metabolomics.github.io/MetMashR/articles/annotate_mixtures.html):
  a case study combining Compound Discoverer and LipidSearch annotations
  of mixtures of chemical standards, and assessing them against the
  known contents of the mixtures.
- [Exploring the MTox700+
  library](https://computational-metabolomics.github.io/MetMashR/articles/exploring_mtox.html):
  a case study combining the MTox700+ database of toxicologically
  relevant metabolites with other databases (e.g. PathBank) to explore
  its chemical and biological coverage.
- [Case study: reimplementing the Metabolites Merging
  Strategy](https://computational-metabolomics.github.io/MetMashR/articles/mms_case_study.html):
  a case study reproducing a published annotation-merging strategy as a
  `MetMashR` workflow.

## Installation

To install the release version from Bioconductor:

\
`if`` ``(``!`[`require`](https://rdrr.io/r/base/library.html)`(`[`"BiocManager"`](https://bioconductor.github.io/BiocManager/)`, quietly ``=`` ``TRUE``)``)`` ``{`\
`    `[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"BiocManager"``)`\
`}`\
\
`BiocManager``::`[`install`](https://bioconductor.github.io/BiocManager/reference/install.html)`(``"MetMashR"``)`

To install the development version from GitHub:

\
`if`` ``(``!`[`require`](https://rdrr.io/r/base/library.html)`(`[`"remotes"`](https://remotes.r-lib.org)`, quietly ``=`` ``TRUE``)``)`` ``{`\
`    `[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"remotes"``)`\
`}`\
\
`remotes``::`[`install_github`](https://remotes.r-lib.org/reference/install_github.html)`(``"computational-metabolomics/MetMashR"``)`

## Quick start

This example uses LipidSearch annotations that are included with the
package. The workflow imports the annotations, keeps only the
higher-confidence ones (grades A and B), then combines duplicate
annotations of the same lipid into a single record.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`MetMashR`](https://computational-metabolomics.github.io/MetMashR/)`)`\
\
`# annotation source: a LipidSearch output file`\
`AT`` ``<-`` `[`ls_source`](https://computational-metabolomics.github.io/MetMashR/reference/ls_source.md)`(`\
`    source ``=`` `[`system.file`](https://rdrr.io/r/base/system.file.html)`(`\
`        ``"extdata/MTox/LS/MTox_2023_HILIC_POS.txt"``,`\
`        package ``=`` ``"MetMashR"`\
`    ``)`\
`)`\
\
`# workflow`\
`WF`` ``<-`\
`    ``# step 1: import the annotations`\
`    `[`import_source`](https://computational-metabolomics.github.io/MetMashR/reference/import_source.md)`(``)`` ``+`\
`    ``# step 2: keep annotations with grade A or B`\
`    `[`filter_labels`](https://computational-metabolomics.github.io/MetMashR/reference/filter_labels.md)`(`\
`        column_name ``=`` ``"Grade"``,`\
`        labels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"A"``, ``"B"``)``,`\
`        mode ``=`` ``"include"`\
`    ``)`` ``+`\
`    ``# step 3: combine records for the same lipid, counting how many there were`\
`    `[`combine_records`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records.md)`(`\
`        group_by ``=`` ``"LipidName"``,`\
`        default_fcn ``=`` `[`fuse_unique`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records_helper_functions.md)`(``separator ``=`` ``"; "``)``,`\
`        fcns ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``count ``=`` `[`count_records`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records_helper_functions.md)`(``)``)`\
`    ``)`\
\
`# apply the workflow to the annotation source`\
`WF`` ``<-`` `[`model_apply`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)`(``WF``, ``AT``)`\
\
`# the processed annotations`\
[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``WF``)`\
[`head`](https://rdrr.io/r/utils/head.html)`(`[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``WF``)``$``data``[``, `[`c`](https://rdrr.io/r/base/c.html)`(``"LipidName"``, ``"Grade"``, ``"count"``)``]``)`\
\
`# the annotations after any individual step, e.g. after filtering`\
[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``WF``[``2``]``)`

The [Using
MetMashR](https://computational-metabolomics.github.io/MetMashR/articles/using_MetMashR.html)
vignette describes these and many other workflow steps in detail.

## Maintainer

Gavin Rhys Lloyd University of Birmingham <g.r.lloyd@bham.ac.uk>

For bug reports and feature requests, please use the [GitHub issue
tracker](https://github.com/computational-metabolomics/MetMashR/issues).

## Citation

To obtain the recommended citation for `MetMashR` in R, run:

\
[`citation`](https://rdrr.io/r/utils/citation.html)`(``"MetMashR"``)`

## License

`MetMashR` is distributed under the GNU General Public License version 3
(GPL-3).
