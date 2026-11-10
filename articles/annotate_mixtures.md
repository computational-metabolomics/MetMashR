# Evaluating metabolite annotations using standard mixtures

**Note:** This vignette is built without network access. Online
databases are read from stored copies included with `MetMashR`, and REST
API lookups use stored response caches with `cache_mode = "offline"`, so
a value that is not in a cache is left as `NA` instead of being queried.
To query the online services yourself, use your own cache and the
default `cache_mode = "update"`. The stored copies were made with
`inst/scripts/make_vignette_snapshots.R`.

\

## Getting started

The latest versions of
*[struct](https://bioconductor.org/packages/3.24/struct)* and `MetMashR`
that are compatible with your current R version can be installed using
BiocManager.

\
`# install BiocManager if not present`\
`if`` ``(``!`[`requireNamespace`](https://rdrr.io/r/base/ns-load.html)`(``"BiocManager"``, quietly ``=`` ``TRUE``)``)`` ``{`\
`    `[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"BiocManager"``)`\
`}`\
\
`# install MetMashR and dependencies`\
`BiocManager``::`[`install`](https://bioconductor.github.io/BiocManager/reference/install.html)`(``"MetMashR"``)`

Once installed you can activate the packages in the usual way:

\
`# load the packages`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`MetMashR`](https://computational-metabolomics.github.io/MetMashR/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplot2`](https://ggplot2.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`structToolbox`](https://github.com/computational-metabolomics/structToolbox)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`DT`](https://github.com/rstudio/DT)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplotify`](https://github.com/GuangchuangYu/ggplotify)`)`

\

## Introduction

A library built from authentic standards can support MSI level 1
identification when a feature matches at least two independent,
orthogonal properties of a standard measured under identical
experimental conditions. This vignette compares identifiers with a
reference list. Identifier agreement alone does not establish level 1
identification (see the [MSI reporting
recommendations](https://pmc.ncbi.nlm.nih.gov/articles/PMC3772505/)).

In this vignette we will explore the annotations generated using
Compound Discoverer (CD) and LipidSearch (LS) for several mixtures of
standards.

As the content of the standard mixtures is known, we can assess the
ability of CD and LS to annotate these metabolites.

\

## Input data

The data collected corresponds to mixtures of high-purity standards
measured using LCMS, with the intention of building an internally
measured library including m/z and retention times for each standard.

Analysis of the standard mixtures resulted in four data tables:
HILIC_NEG, HILIC_POS, LIPIDS_POS and LIPIDS_NEG. We will loosely refer
to these as “assays”.

All four assays were used as input to both Compound Discoverer (CD) and
LipidSearch (LS) software, which are software tools commonly used to
annotation LCMS datasets.

\

## Importing the annotations

`MetMashR` includes `cd_source` and `ls_source` objects. These objects
read the output files from CD and LS and parse them into an
`annotation_table` object.

Importing the annotation tables isn’t always enough; sometimes they need
to be cleaned or processed further. We therefore define two `MetMashR`
workflows, one for CD and one for LS, in which we import the tables and
then apply source-specific cleaning.

\

### Importing Compound Discoverer annotations

The CD file format expected by `cd_source` is Excel format.

For the CD `MetMashR` workflow we would like to:

1.  Import CD annotations and convert to `annotation_table` format
2.  Filter to only include “Full match” annotations
3.  Resolve duplicates

When we import the annotations, we add a column indicating which assay
the annotations are associated with, include a `tag` for each row
indicating both the source and the assay. This will be useful later when
processing the combined tables.

The resolution of duplicates is needed because CD might assign the same
metabolite + adduct to multiple peaks. Here we choose the match with
highest mzcloud score using the `select_max` helper function with the
combine_records\` object. This object is a wrapper for
\[dplyr::reframe()\].

\
`# prepare workflow`\
`M`` ``<-`` `[`import_source`](https://computational-metabolomics.github.io/MetMashR/reference/import_source.md)`(``)`` ``+`\
`    `[`add_labels`](https://computational-metabolomics.github.io/MetMashR/reference/add_labels.md)`(`\
`        labels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`            assay ``=`` ``"placeholder"``, ``# will be replaced later`\
`            source_name ``=`` ``"CD"`\
`        ``)`\
`    ``)`` ``+`\
`    `[`filter_labels`](https://computational-metabolomics.github.io/MetMashR/reference/filter_labels.md)`(`\
`        column_name ``=`` ``"compound_match"``,`\
`        labels ``=`` ``"Full match"``,`\
`        mode ``=`` ``"include"`\
`    ``)`` ``+`\
`    `[`filter_range`](https://computational-metabolomics.github.io/MetMashR/reference/filter_range.md)`(`\
`        column_name ``=`` ``"library_ppm_diff"``,`\
`        upper_limit ``=`` ``2``,`\
`        lower_limit ``=`` ``-``2``,`\
`        equal_to ``=`` ``FALSE`\
`    ``)`` ``+`\
`    `[`combine_records`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records.md)`(`\
`        group_by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"compound"``, ``"ion"``)``,`\
`        default_fcn ``=`` `[`select_max`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records_helper_functions.md)`(`\
`            max_col ``=`` ``"mzcloud_score"``,`\
`            keep_NA ``=`` ``FALSE``,`\
`            use_abs ``=`` ``TRUE`\
`        ``)`\
`    ``)`\
\
`# place to store results`\
`CD`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(``)`\
`for`` ``(``assay`` ``in`` `[`c`](https://rdrr.io/r/base/c.html)`(``"HILIC_NEG"``, ``"HILIC_POS"``, ``"LIPIDS_NEG"``, ``"LIPIDS_POS"``)``)`` ``{`\
`    ``# prepare source`\
`    ``AT`` ``<-`` `[`cd_source`](https://computational-metabolomics.github.io/MetMashR/reference/cd_source.md)`(`\
`        source ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`            `[`system.file`](https://rdrr.io/r/base/system.file.html)`(`\
`                `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"extdata/MTox/CD/"``, ``assay``, ``".xlsx"``)``,`\
`                package ``=`` ``"MetMashR"`\
`            ``)``,`\
`            `[`system.file`](https://rdrr.io/r/base/system.file.html)`(`\
`                `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"extdata/MTox/CD/"``, ``assay``, ``"_comp.xlsx"``)``,`\
`                package ``=`` ``"MetMashR"`\
`            ``)`\
`        ``)``,`\
`        tag ``=`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"CD_"``, ``assay``)`\
`    ``)`\
\
`    ``# update labels in workflow`\
`    ``M``[``2``]``$``labels``$``assay`` ``<-`` ``assay`\
\
`    ``# apply workflow to source`\
`    ``CD``[[``assay``]``]`` ``<-`` `[`model_apply`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)`(``M``, ``AT``)`\
`}`

The `CD` variable is now a list containing the workflow for each assay.

The default output of each workflow is a processed `lcms_table`, which
is an extension of `annotation_table` that requires both an m/z and a
retention time column to be defined.

A summary of the table for e.g. the HILIC_NEG assay can be displayed on
the console:

\
[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``CD``$``HILIC_NEG``)`\
`#> A "cd_source" object`\
`#> --------------------`\
`#> name:          LCMS table`\
`` #> description:   An LCMS table extends [`annotation_table()`] to represent annotation data for an LCMS ``\
`#>                  experiment. Columns representing m/z and retention time are required for an`\
`` #>                  `lcms_table`. ``\
`#> input params:  sheets `\
`#> annotations:   219 rows x 21 columns`

The HILIC_NEG table is shown below.

`MetMashR` workflows store the output after each step. We can use this
to explore the impact of different workflow steps. For example, we can
display the different compound matches present before and after
filtering as pie charts. The pie charts count annotation rows.

First, we create the pie chart object and specify some parameters.

\
`C`` ``<-`` `[`annotation_pie_chart`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_pie_chart.md)`(`\
`    factor_name ``=`` ``"compound_match"``,`\
`    label_rotation ``=`` ``FALSE``,`\
`    label_location ``=`` ``"outside"``,`\
`    legend ``=`` ``TRUE``,`\
`    label_type ``=`` ``"percent"``,`\
`    centre_radius ``=`` ``0.5``,`\
`    centre_label ``=`` ``".total"`\
`)`

Now we create the plots using `chart_plot` and add some additional
settings using `ggplot2`, and arrange the plots using `cowplot`.

Note that we use square brackets to index the step of the workflow we
want to access e.g. `HILIC_NEG[3]`.

\
`# plot individual charts`\
`g1`` ``<-`` `[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``CD``$``HILIC_NEG``[``3``]``)``)`` ``+`\
`    `[`ggtitle`](https://ggplot2.tidyverse.org/reference/labs.html)`(``"Compound matches\nafter filtering"``)`` ``+`\
`    `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``plot.title ``=`` `[`element_text`](https://ggplot2.tidyverse.org/reference/element.html)`(``hjust ``=`` ``0.5``)``)`\
\
\
`g2`` ``<-`` `[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``CD``$``HILIC_NEG``[``2``]``)``)`` ``+`\
`    `[`ggtitle`](https://ggplot2.tidyverse.org/reference/labs.html)`(``"Compound matches\nbefore filtering"``)`` ``+`\
`    `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``plot.title ``=`` `[`element_text`](https://ggplot2.tidyverse.org/reference/element.html)`(``hjust ``=`` ``0.5``)``)`\
\
\
`# get legend`\
`leg`` ``<-`` ``cowplot``::`[`get_legend`](https://wilkelab.org/cowplot/reference/get_legend.html)`(``g2``)`\
\
`# layout`\
`cowplot``::`[`plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html)`(`\
`    ``g2`` ``+`` `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``legend.position ``=`` ``"none"``)``,`\
`    ``g1`` ``+`` `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``legend.position ``=`` ``"none"``)``,`\
`    ``leg``,`\
`    nrow ``=`` ``1``,`\
`    rel_widths ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``1``, ``0.5``)`\
`)`

![](annotate_mixtures_files/figure-html/annotate-mixtures-9-1.png)

It is clear from these plots that the filter has removed all annotations
without a “full match”.

\

We can assess the quality of the annotations be examining the histogram
of ppm errors between the MS2 peaks and the library.

If there is a wide distribution then this may indicate false positives.
If the distribution is offset from zero then this indicates some m/z
drift is present.

\
`C`` ``<-`` `[`annotation_histogram`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_histogram.md)`(`\
`    factor_name ``=`` ``"library_ppm_diff"``, vline ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``2``, ``2``)`\
`)`\
`G`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(``)`\
`G``$``HILIC_NEG`` ``<-`` `[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``CD``$``HILIC_NEG``[``3``]``)``)`\
`G``$``HILIC_POS`` ``<-`` `[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``CD``$``HILIC_POS``[``3``]``)``)`\
\
`cowplot``::`[`plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html)`(``plotlist ``=`` ``G``, labels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"HILIC_NEG"``, ``"HILIC_POS"``)``)`

![](annotate_mixtures_files/figure-html/annotate-mixtures-10-1.png)

Note that we plotted the distribution based on the step before filtering
by range, and set the vertical red lines equal to the range filter so
that we can see which parts of the histogram are affected by the range
filter.

### Importing LipidSearch annotations

The file format expected by `ls_source` is a tab-delimited text file.

For the `MetMashR` workflow we would like to:

1.  Import the LS annotations and convert to `annotation_table` format
2.  Filter the annotations to only include grades A and B
3.  Extract the adduct from the `LipidIon` column
4.  Resolve duplicates

When we import the annotations, we add a column indicating which assay
and source the annotations are associated with. We also include a `tag`
for the table. This will be useful later when processing the combined
tables.

LipidSearch reports the adduct as part of the `LipidIon` column (e.g. 
“PC(16:0_18:1)+H”). We use
[`compute_column()`](https://computational-metabolomics.github.io/MetMashR/reference/compute_column.md)
to extract the adduct into a new `LipidAdduct` column, written in the
same notation as the Compound Discoverer `ion` column
(e.g. “\[M+H\]+1”), so that the two can be combined later.

For LS we resolve duplicates by selecting the annotation with the
smallest ppm error. We use the `combine_records` object with the
`select_min` helper function to do this.

\
`# prepare workflow`\
`M`` ``<-`` `[`import_source`](https://computational-metabolomics.github.io/MetMashR/reference/import_source.md)`(``)`` ``+`\
`    `[`add_labels`](https://computational-metabolomics.github.io/MetMashR/reference/add_labels.md)`(`\
`        labels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`            assay ``=`` ``"placeholder"``, ``# will be replaced later`\
`            source_name ``=`` ``"LS"`\
`        ``)`\
`    ``)`` ``+`\
`    ``# extract the adduct from LipidIon (e.g. "PC(16:0_18:1)+H" -> "+H") and`\
`    ``# write it in the same notation as the Compound Discoverer "ion" column`\
`    `[`compute_column`](https://computational-metabolomics.github.io/MetMashR/reference/compute_column.md)`(`\
`        input_columns ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"LipidIon"``, ``"LipidName"``, ``"assay"``)``,`\
`        output_column ``=`` ``"LipidAdduct"``,`\
`        fcn ``=`` ``function``(``x``)`` ``{`\
`            ``a`` ``<-`` `[`substring`](https://rdrr.io/r/base/substr.html)`(``x``$``LipidIon``, `[`nchar`](https://rdrr.io/r/base/nchar.html)`(``x``$``LipidName``)`` ``+`` ``1``)`\
`            ``a``[``a`` ``==`` ``"+CH3COO"``]`` ``<-`` ``"-H+HAc"`\
`            ``a``[``a`` ``==`` ``"+HCOO"``]`` ``<-`` ``"+FA-H"`\
`            ``charge`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`[`grepl`](https://rdrr.io/r/base/grep.html)`(``"NEG"``, ``x``$``assay``)``, ``"-1"``, ``"+1"``)`\
`            `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"[M"``, ``a``, ``"]"``, ``charge``)`\
`        ``}`\
`    ``)`` ``+`\
`    `[`filter_labels`](https://computational-metabolomics.github.io/MetMashR/reference/filter_labels.md)`(`\
`        column_name ``=`` ``"Grade"``,`\
`        labels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"A"``, ``"B"``)``,`\
`        mode ``=`` ``"include"`\
`    ``)`` ``+`\
`    `[`combine_records`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records.md)`(`\
`        group_by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"LipidIon"``)``,`\
`        default_fcn ``=`` `[`select_min`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records_helper_functions.md)`(`\
`            min_col ``=`` ``"library_ppm_diff"``,`\
`            keep_NA ``=`` ``FALSE``,`\
`            use_abs ``=`` ``TRUE`\
`        ``)`\
`    ``)`\
\
`# place to store results`\
`LS`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(``)`\
`for`` ``(``assay`` ``in`` `[`c`](https://rdrr.io/r/base/c.html)`(``"HILIC_NEG"``, ``"HILIC_POS"``, ``"LIPIDS_NEG"``, ``"LIPIDS_POS"``)``)`` ``{`\
`    ``# prepare source`\
`    ``AT`` ``<-`` `[`ls_source`](https://computational-metabolomics.github.io/MetMashR/reference/ls_source.md)`(`\
`        source ``=`` `[`system.file`](https://rdrr.io/r/base/system.file.html)`(`\
`            `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"extdata/MTox/LS/MTox_2023_"``, ``assay``, ``".txt"``)``,`\
`            package ``=`` ``"MetMashR"`\
`        ``)``,`\
`        tag ``=`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"LS_"``, ``assay``)`\
`    ``)`\
\
`    ``# update labels in workflow`\
`    ``M``[``2``]``$``labels``$``assay`` ``<-`` ``assay`\
\
`    ``# apply workflow to source`\
`    ``LS``[[``assay``]``]`` ``<-`` `[`model_apply`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)`(``M``, ``AT``)`\
`}`

The `LS` variable is now a list containing the workflow for each assay.

A summary of the table for e.g. the LIPIDS_NEG assay can be displayed on
the console:

\
[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``LS``$``LIPIDS_NEG``)`\
`#> A "ls_source" object`\
`#> --------------------`\
`#> name:          LCMS table`\
`` #> description:   An LCMS table extends [`annotation_table()`] to represent annotation data for an LCMS ``\
`#>                  experiment. Columns representing m/z and retention time are required for an`\
`` #>                  `lcms_table`. ``\
`#> input params:  mz_column, rt_column `\
`#> annotations:   16 rows x 15 columns`

The LIPIDS_NEG table is shown below.

`MetMashR` workflows store the output after each step. We can use this
to explore the impact of different workflow steps. For example, we can
display the different Grades present before and after filtering as pie
charts of annotation rows.

\
`C`` ``<-`` `[`annotation_pie_chart`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_pie_chart.md)`(`\
`    factor_name ``=`` ``"Grade"``,`\
`    label_rotation ``=`` ``FALSE``,`\
`    label_location ``=`` ``"outside"``,`\
`    label_type ``=`` ``"percent"``,`\
`    legend ``=`` ``FALSE``,`\
`    centre_radius ``=`` ``0.5``,`\
`    centre_label ``=`` ``".total"`\
`)`\
`g1`` ``<-`` `[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``LS``$``LIPIDS_NEG``)``)`` ``+`\
`    `[`ggtitle`](https://ggplot2.tidyverse.org/reference/labs.html)`(``"Grades after filtering"``)`` ``+`\
`    `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``plot.margin ``=`` `[`unit`](https://rdrr.io/r/grid/unit.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``1.5``, ``1``, ``1.5``)``, ``"cm"``)``)`\
\
`g2`` ``<-`` `[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``LS``$``LIPIDS_NEG``[``1``]``)``)`` ``+`\
`    `[`ggtitle`](https://ggplot2.tidyverse.org/reference/labs.html)`(``"Grades before filtering"``)`` ``+`\
`    `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``plot.margin ``=`` `[`unit`](https://rdrr.io/r/grid/unit.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``1.5``, ``1``, ``1.5``)``, ``"cm"``)``)`\
\
\
`cowplot``::`[`plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html)`(``g2``, ``g1``, nrow ``=`` ``1``, align ``=`` ``"v"``)`

![](annotate_mixtures_files/figure-html/annotate-mixtures-14-1.png)\

## Exploratory analysis of annotation sources

The annotations imported from each source are interesting to explore
graphically “within source”. We will draw a comparison “between sources”
later.

In this example we generate Venn diagrams by providing several
`annotation_table` inputs to the `chart_plot` function for an
`annotation_venn_chart`. This allows us to compare columns of
e.g. compound names present in each table. The Venn and UpSet charts
count distinct names, not annotation rows.

Here, we compare the compound names for each assay within each source,
to see if there is any overlap i.e. the same metabolite detecting in
several assays.

\
`# prepare venn chart object`\
`C`` ``<-`` `[`annotation_venn_chart`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_venn_chart.md)`(`\
`    factor_name ``=`` ``"compound"``,`\
`    line_colour ``=`` ``"white"``,`\
`    fill_colour ``=`` ``".group"``,`\
`    legend ``=`` ``TRUE``,`\
`    labels ``=`` ``FALSE`\
`)`\
\
`## plot`\
`# get all CD tables`\
`cd`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``CD``, ``predicted``)`\
`g1`` ``<-`` `[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, ``cd``)`` ``+`` `[`ggtitle`](https://ggplot2.tidyverse.org/reference/labs.html)`(``"Compounds in CD per assay"``)`\
\
`# get all LS tables`\
`C``$``factor_name`` ``<-`` ``"LipidName"`\
`ls`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``LS``, ``predicted``)`\
`g2`` ``<-`` `[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, ``ls``)`` ``+`` `[`ggtitle`](https://ggplot2.tidyverse.org/reference/labs.html)`(``"Compounds in LS per assay"``)`\
\
`# prepare upset object`\
`C2`` ``<-`` `[`annotation_upset_chart`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_upset_chart.md)`(`\
`    factor_name ``=`` ``"compound"``,`\
`    nintersects ``=`` ``10``,`\
`)`\
`g3`` ``<-`` `[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C2``, ``cd``)`\
`C2``$``factor_name`` ``<-`` ``"LipidName"`\
`g4`` ``<-`` `[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C2``, ``ls``)`\
\
`# layout`\
`cowplot``::`[`plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html)`(``g1``, ``g2``, `[`as.ggplot`](https://rdrr.io/pkg/ggplotify/man/as.ggplot.html)`(``g3``)``, `[`as.ggplot`](https://rdrr.io/pkg/ggplotify/man/as.ggplot.html)`(``g4``)``, nrow ``=`` ``2``)`

![](annotate_mixtures_files/figure-html/annotate-mixtures-15-1.png)

The diagram for CD shows the largest amount of overlap is between assays
with the same ion mode.

For LS the number of annotations is quite small, so the diagram is less
informative. However, we are clearly detecting a larger number of
annotations in the LIPIDS assays, which is to be expected as the LS
software is designed to annotate lipids molecules.

Here the `annotation_venn_chart` and `annotation_upset_chart` objects
were used to compare the same column across several `annotation_tables`.
The same objects can also compare groups from within the same table,
which we will explore later.

First, we need to combine the CD and LS tables.

## Combining annotation sources

In this workflow step we combine the imported assay tables from each
assay and annotation source vertically into a single annotation table.

The
[`combine_sources()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_sources.md)
object can be used for this step. Combining tables from the same source
(e.g. the CD table for each assay) is straight forward as all tables
have the same columns.

When combining different sources the
[`combine_sources()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_sources.md)
object provides input parameters that allow you to combine and select
columns from different sources into new columns with the same
information. For example the `adduct` column in CD is called “ion” and
in LS it is the “LipidAdduct” column we extracted from “LipidIon”; we
can combine these columns into a new column called “adduct”.

Here we specify that all columns should be retained from both tables,
padding with NA if not present; columns with the same name are
automatically merged.

\
`# get all the cleaned annotation tables in one list`\
`all_source_tables`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``CD``, ``LS``)``, ``predicted``)`\
\
`# prepare to merge`\
`combine_workflow`` ``<-`\
`    `[`combine_sources`](https://computational-metabolomics.github.io/MetMashR/reference/combine_sources.md)`(`\
`        source_list ``=`` ``all_source_tables``,`\
`        matching_columns ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`            name ``=`` ``"LipidName"``,`\
`            name ``=`` ``"compound"``,`\
`            adduct ``=`` ``"ion"``,`\
`            adduct ``=`` ``"LipidAdduct"`\
`        ``)``,`\
`        keep_cols ``=`` ``".all"``,`\
`        source_col ``=`` ``"annotation_table"``,`\
`        exclude_cols ``=`` ``NULL``,`\
`        tag ``=`` ``"combined"`\
`    ``)`\
\
`# merge`\
`combine_workflow`` ``<-`` `[`model_apply`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)`(``combine_workflow``, `[`lcms_table`](https://computational-metabolomics.github.io/MetMashR/reference/lcms_table.md)`(``)``)`\
\
`# show`\
[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``combine_workflow``)`\
`#> A "annotation_source" object`\
`#> ----------------------------`\
`#> name:          combined`\
`#> description:   A source created by combining two or more sources`\
`#> input params:  tag, data, source `\
`#> annotations:   912 rows x 29 columns`

Now that the tables have been combined we can explore the table using
charts. For example, we visualise the number of annotations for each
assay from both sources.

\
`C`` ``<-`` `[`annotation_pie_chart`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_pie_chart.md)`(`\
`    factor_name ``=`` ``"assay"``,`\
`    label_rotation ``=`` ``FALSE``,`\
`    label_location ``=`` ``"outside"``,`\
`    label_type ``=`` ``"percent"``,`\
`    legend ``=`` ``TRUE``,`\
`    centre_radius ``=`` ``0.5``,`\
`    centre_label ``=`` ``".total"`\
`)`\
\
[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``combine_workflow``)``)`` ``+`\
`    `[`ggtitle`](https://ggplot2.tidyverse.org/reference/labs.html)`(``"Annotations per assay"``)`` ``+`\
`    `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``plot.margin ``=`` `[`unit`](https://rdrr.io/r/grid/unit.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``1.5``, ``1``, ``1.5``)``, ``"cm"``)``)`` ``+`\
`    `[`guides`](https://ggplot2.tidyverse.org/reference/guides.html)`(``fill ``=`` `[`guide_legend`](https://ggplot2.tidyverse.org/reference/guide_legend.html)`(``title ``=`` ``"Assay"``)``)`

![](annotate_mixtures_files/figure-html/annotate-mixtures-17-1.png)

In this next example we compare the number of annotations from each
source.

\
`# change to plot source_name column`\
`C``$``factor_name`` ``<-`` ``"source_name"`\
[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``combine_workflow``)``)`` ``+`\
`    `[`ggtitle`](https://ggplot2.tidyverse.org/reference/labs.html)`(``"Annotations per source"``)`` ``+`\
`    `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``plot.margin ``=`` `[`unit`](https://rdrr.io/r/grid/unit.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``1.5``, ``1``, ``1.5``)``, ``"cm"``)``)`` ``+`\
`    `[`guides`](https://ggplot2.tidyverse.org/reference/guides.html)`(``fill ``=`` `[`guide_legend`](https://ggplot2.tidyverse.org/reference/guide_legend.html)`(``title ``=`` ``"Source"``)``)`

![](annotate_mixtures_files/figure-html/annotate-mixtures-18-1.png)

## Adding identifiers

Ultimately we would like to compare the annotations detected using our
software sources to the list of standard we included in the samples.
Comparing the two tables using metabolite names is less than ideal,
because different sources might use different synonyms for the same
molecular structure.

To overcome this it is much better to compare the tables using molecular
identifiers such as InChIKey, which are unique to the molecule.
`MetMashR` includes a number of workflow steps that allow us to look up
identifiers from various databases, either stored locally, or in online
databases such as PubChem by using their REST API.

For this vignette we use cached results so that we don’t overburden the
api; in practice you would create your own.

\
`# import cached results`\
`inchikey_cache`` ``<-`` `[`rds_database`](https://computational-metabolomics.github.io/MetMashR/reference/rds_database.md)`(`\
`    source ``=`` `[`file.path`](https://rdrr.io/r/base/file.path.html)`(`\
`        `[`system.file`](https://rdrr.io/r/base/system.file.html)`(``"cached"``, package ``=`` ``"MetMashR"``)``,`\
`        ``"pubchem_inchikey_mtox_cache.rds"`\
`    ``)`\
`)`\
\
`id_workflow`` ``<-`\
`    `[`pubchem_property_lookup`](https://computational-metabolomics.github.io/MetMashR/reference/pubchem_property_lookup.md)`(`\
`        query_column ``=`` ``"name"``,`\
`        search_by ``=`` ``"name"``,`\
`        suffix ``=`` ``""``,`\
`        property ``=`` ``"InChIKey"``,`\
`        records ``=`` ``"best"``,`\
`        cache ``=`` ``inchikey_cache``,`\
`        cache_mode ``=`` ``"offline"`\
`    ``)`\
\
`id_workflow`` ``<-`` `[`model_apply`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)`(``id_workflow``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``combine_workflow``)``)`

\

## Improving ID coverage

The ID’s obtained in the previous section were obtained by queries based
on the molecule name.

Molecule names can contain a number of special characters, and follow
different nomenclatures when constructing the name, as well as
abbreviations and naming conventions.

It useful therefore to apply some kind of “molecule name normalisation”
to account for these properties of molecule names.

We can use the `normalise_strings` MetMashR object to do this. This
object has a `dictionary` parameter that takes the form of a list of
lists.

Each sub-list contains a pattern to be matched and a replacement. In the
example workflow below we include a number of definitions in our
dictionary:

- Update compounds starting with “NP” to start “Compound NP” as this is
  how they are recorded in PubChem.
- Replace any molecule name containing a ? with NA, as this indicates
  ambiguity in the annotation.
- Remove abbreviations from molecule names e.g. “adenosine triphosphate
  (ATP)” should have the “(ATP)” part removed.
- Replace some shorthand names with more formal names that are more
  likely to result in a match to a PubChem compound.
- Remove optical-rotation notation, for example D-(+)-Glucose becomes
  D-Glucose.
- Remove racemic prefixes such as “DL-” or “(±)”, which indicate a
  mixture of both enantiomers.

The original names are kept in the `name` column; the changed names are
written to a new `normalised_name` column. - Replace Greek characters
with their Romanised names.

Both the Greek and racemic dictionaries are provided by MetMashR for
convenience.

In steps 2 and 3 of the workflow we submit these normalised names to the
PubChem API and to OPSIN. By utlilising serveral API’s we can maximise
the number of molecules we obtain an InChIKey for.

In the final step we merge the three columns of identifiers, giving
priority to OPSIN, which is based on deconstructing the molecule name
into its component parts. If there is no result from OPSIN then a
PubChem search based on the normalised names is prioritised over a
PubChem search using the non-normalised names.

\
`# prepare cached results for vignette`\
`inchikey_cache2`` ``<-`` `[`rds_database`](https://computational-metabolomics.github.io/MetMashR/reference/rds_database.md)`(`\
`    source ``=`` `[`file.path`](https://rdrr.io/r/base/file.path.html)`(`\
`        `[`system.file`](https://rdrr.io/r/base/system.file.html)`(``"cached"``, package ``=`` ``"MetMashR"``)``,`\
`        ``"pubchem_inchikey_mtox_cache2.rds"`\
`    ``)`\
`)`\
`inchikey_cache3`` ``<-`` `[`rds_database`](https://computational-metabolomics.github.io/MetMashR/reference/rds_database.md)`(`\
`    source ``=`` `[`file.path`](https://rdrr.io/r/base/file.path.html)`(`\
`        `[`system.file`](https://rdrr.io/r/base/system.file.html)`(``"cached"``, package ``=`` ``"MetMashR"``)``,`\
`        ``"pubchem_inchikey_mtox_cache3.rds"`\
`    ``)`\
`)`\
\
`N`` ``<-`` `[`normalise_strings`](https://computational-metabolomics.github.io/MetMashR/reference/normalise_strings.md)`(`\
`    search_column ``=`` ``"name"``,`\
`    output_column ``=`` ``"normalised_name"``,`\
`    dictionary ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`        ``# custom dictionary`\
`        `[`list`](https://rdrr.io/r/base/list.html)`(`\
`            ``# replace "NP" with "Compound NP"`\
`            `[`list`](https://rdrr.io/r/base/list.html)`(``pattern ``=`` ``"^NP-"``, replace ``=`` ``"Compound NP-"``)``,`\
`            ``# replace ? with NA, since this is ambiguous`\
`            `[`list`](https://rdrr.io/r/base/list.html)`(``pattern ``=`` ``"?"``, replace ``=`` ``NA``, fixed ``=`` ``TRUE``)``,`\
`            ``# remove terms in trailing brackets e.g." (ATP)"`\
`            `[`list`](https://rdrr.io/r/base/list.html)`(``pattern ``=`` ``"\\ \\([^\\)]*\\)$"``, replace ``=`` ``""``)``,`\
`            ``# replace known abbreviations`\
`            `[`list`](https://rdrr.io/r/base/list.html)`(`\
`                pattern ``=`` ``"(+/-)9-HpODE"``,`\
`                replace ``=`` ``"9-hydroperoxy-10E,12Z-octadecadienoic acid"``,`\
`                fixed ``=`` ``TRUE`\
`            ``)``,`\
`            `[`list`](https://rdrr.io/r/base/list.html)`(`\
`                pattern ``=`` ``"(+/-)19(20)-DiHDPA"``,`\
`                replace ``=`\
`                    ``"19,20-dihydroxy-4Z,7Z,10Z,13Z,16Z-docosapentaenoic acid"``,`\
`                fixed ``=`` ``TRUE`\
`            ``)`\
`        ``)``,`\
`        ``# replace greek characters`\
`        ``greek_dictionary``,`\
`        ``# remove optical-rotation and racemic notation`\
`        ``racemic_dictionary`\
`    ``)`\
`)`` ``+`\
`    `[`pubchem_property_lookup`](https://computational-metabolomics.github.io/MetMashR/reference/pubchem_property_lookup.md)`(`\
`        query_column ``=`` ``"normalised_name"``,`\
`        search_by ``=`` ``"name"``,`\
`        suffix ``=`` ``"_norm"``,`\
`        property ``=`` ``"InChIKey"``,`\
`        records ``=`` ``"best"``,`\
`        cache ``=`` ``inchikey_cache2``,`\
`        cache_mode ``=`` ``"offline"`\
`    ``)`` ``+`\
`    `[`opsin_lookup`](https://computational-metabolomics.github.io/MetMashR/reference/opsin_lookup.md)`(`\
`        query_column ``=`` ``"normalised_name"``,`\
`        suffix ``=`` ``"_opsin"``,`\
`        output ``=`` ``"stdinchikey"``,`\
`        cache ``=`` ``inchikey_cache3``,`\
`        cache_mode ``=`` ``"offline"`\
`    ``)`` ``+`\
`    `[`prioritise_columns`](https://computational-metabolomics.github.io/MetMashR/reference/prioritise_columns.md)`(`\
`        column_names ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"stdinchikey_opsin"``, ``"InChIKey_norm"``, ``"InChIKey"``)``,`\
`        output_name ``=`` ``"inchikey"``,`\
`        source_name ``=`` ``"inchikey_source"``,`\
`        clean ``=`` ``TRUE`\
`    ``)`\
\
`N`` ``<-`` `[`model_apply`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)`(``N``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``id_workflow``)``)`

We can explore the impact of these workflow steps using Venn and Pie
charts to compare the results before/after the workflow.

In this Venn diagram we show the overlap between the distinct InChIKey
identifiers obtained from the three queries. Normalising the names
resulted in 38 distinct identifiers that we were unable to obtain
without normalisation. The use of OPSIN then added a further 28 distinct
identifiers not found by either PubChem query. The column of combined
identifiers therefore contains 340 distinct, non-missing identifiers.

\
`# venn inchikey columns`\
`C`` ``<-`` `[`annotation_venn_chart`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_venn_chart.md)`(`\
`    factor_name ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"InChIKey"``, ``"InChIKey_norm"``, ``"stdinchikey_opsin"``)``,`\
`    line_colour ``=`` ``"white"``,`\
`    fill_colour ``=`` ``".group"``,`\
`    legend ``=`` ``TRUE``,`\
`    labels ``=`` ``FALSE`\
`)`\
[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``N``[``3``]``)``)`` ``+`\
`    `[`guides`](https://ggplot2.tidyverse.org/reference/guides.html)`(`\
`        fill ``=`` `[`guide_legend`](https://ggplot2.tidyverse.org/reference/guide_legend.html)`(``title ``=`` ``"Source"``)``,`\
`        colour ``=`` `[`guide_legend`](https://ggplot2.tidyverse.org/reference/guide_legend.html)`(``title ``=`` ``"Source"``)`\
`    ``)`` ``+`\
`    `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``plot.margin ``=`` `[`unit`](https://rdrr.io/r/grid/unit.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``1.5``, ``1``, ``1.5``)``, ``"cm"``)``)`

![Distinct InChIKeys returned by each
query.](annotate_mixtures_files/figure-html/annotate-mixtures-21-1.png)

Distinct InChIKeys returned by each query.

Due to complexity, Venn charts are limited to 7 sets. UpSet plots are an
alternative chart than can accomodate many more sets. Each veritcal bar
in the UpSet plot corresponds to a region in the venn diagram.

\
`# upset inchikey columns`\
`C`` ``<-`` `[`annotation_upset_chart`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_upset_chart.md)`(`\
`    factor_name ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"InChIKey"``, ``"InChIKey_norm"``, ``"stdinchikey_opsin"``)``,`\
`    nintersects ``=`` ``10`\
`)`\
[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``N``[``3``]``)``)`

![](annotate_mixtures_files/figure-html/annotate-mixtures-22-1.png)

In the next chart we visualise which query supplied the selected
identifier for each annotation row, after prioritisation has taken
place. Rows with no identifier from any query have no source and are
shown as NA.

These counts are annotation rows, not distinct identifiers; the same
identifier can appear in several rows if e.g. the same molecule was
annotated more than once. Of the 912 annotation rows, 790 have an
identifier, and 2 of these (0.3%) came from the PubChem query based on
the original name.

\
`# pie source of inchikey`\
`C`` ``<-`` `[`annotation_pie_chart`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_pie_chart.md)`(`\
`    factor_name ``=`` ``"inchikey_source"``,`\
`    label_rotation ``=`` ``FALSE``,`\
`    label_location ``=`` ``"outside"``,`\
`    label_type ``=`` ``"percent"``,`\
`    legend ``=`` ``TRUE``,`\
`    centre_radius ``=`` ``0.5``,`\
`    centre_label ``=`` ``".total"``,`\
`    count_na ``=`` ``TRUE`\
`)`\
[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``N``)``)`` ``+`\
`    `[`guides`](https://ggplot2.tidyverse.org/reference/guides.html)`(`\
`        fill ``=`` `[`guide_legend`](https://ggplot2.tidyverse.org/reference/guide_legend.html)`(``title ``=`` ``"Source"``)``,`\
`        colour ``=`` `[`guide_legend`](https://ggplot2.tidyverse.org/reference/guide_legend.html)`(``title ``=`` ``"Source"``)`\
`    ``)`` ``+`\
`    `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``plot.margin ``=`` `[`unit`](https://rdrr.io/r/grid/unit.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``1.5``, ``1``, ``1.5``)``, ``"cm"``)``)`

![Annotation rows by the query that supplied the selected InChIKey; NA
means no identifier was
found.](annotate_mixtures_files/figure-html/annotate-mixtures-23-1.png)

Annotation rows by the query that supplied the selected InChIKey; NA
means no identifier was found.

This example removes rows whose selected identifier was found only by
PubChem using the original name. This is a chosen filtering rule; it has
not been shown to produce more accurate identifiers. It removes 2 of the
912 rows; rows with no identifier are kept. The unfiltered lookup
results are kept in `predicted(N)`.

\
`# prepare workflow`\
`FL`` ``<-`` `[`filter_labels`](https://computational-metabolomics.github.io/MetMashR/reference/filter_labels.md)`(`\
`    column_name ``=`` ``"inchikey_source"``,`\
`    labels ``=`` ``"^InChIKey$"``, ``# exact match; labels are regular expressions`\
`    mode ``=`` ``"exclude"`\
`)`\
`# apply`\
`FF`` ``<-`` `[`model_apply`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)`(``FL``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``N``)``)`\
\
`# print summary`\
[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``FF``)`\
`#> A "annotation_source" object`\
`#> ----------------------------`\
`#> name:          combined`\
`#> description:   A source created by combining two or more sources`\
`#> input params:  tag, data, source `\
`#> annotations:   910 rows x 34 columns`

\

## Comparison with the true mixtures

In this section we compare the annotated features with the table of
standards known to be included in each of the mixtures.

\

### Importing the standard mixture tables

The first step is to import the tables of standards for each mixture.
The data has already been saved as an RDS file so we can use
`rds_database` to import it.

\
`# prepare object`\
`R`` ``<-`` `[`rds_database`](https://computational-metabolomics.github.io/MetMashR/reference/rds_database.md)`(`\
`    source ``=`` `[`file.path`](https://rdrr.io/r/base/file.path.html)`(`\
`        `[`system.file`](https://rdrr.io/r/base/system.file.html)`(``"extdata"``, package ``=`` ``"MetMashR"``)``,`\
`        ``"standard_mixtures.rds"`\
`    ``)``,`\
`    .writable ``=`` ``FALSE`\
`)`\
\
`# read`\
`R`` ``<-`` `[`read_source`](https://computational-metabolomics.github.io/MetMashR/reference/read_source.md)`(``R``)`

The standards table contains a list of metabolites and the the mixture
they were included in. It also contains some manually curated data
providing m/z and retention time of the metabolite, which assay it was
observed in and the adduct.

\

### Identifiers for the standards

The standards table provides HMDB identifiers for each metabolite, but
our preference is to work with InChIkey. So our first task is to obtain
InChiKey for the standards.

The standards are based on the MTox700+ database, so we can import
MTox700+ (here a stored copy of the database downloaded by
[`MTox700plus_database()`](https://computational-metabolomics.github.io/MetMashR/reference/MTox700plus_database.md))
and use it to obtain InChIKey identifiers by matching the HMDB
identifiers. An alternative might be to use `hmdb_lookup` and/or
`pubchem_property_lookup`.

Each row of the standards table is one standard in one assay. Before
looking up identifiers we remove rows where the retention time, the
number of MS/MS scans or the mzCloud identifier is missing, and rows
with no MS/MS scans (the
[`filter_range()`](https://computational-metabolomics.github.io/MetMashR/reference/filter_range.md)
step with `equal_to = TRUE` removes values equal to the lower limit of
0). These rows describe standards that were not observed in that assay,
or have no MS/MS spectrum or mzCloud entry that the annotation software
could have matched. The comparison with the annotations below uses only
the standards left after these filters.

\
`# convert standard mixtures to source, then get inchikey from MTox700+`\
`SM`` ``<-`` `[`import_source`](https://computational-metabolomics.github.io/MetMashR/reference/import_source.md)`(``)`` ``+`\
`    `[`filter_na`](https://computational-metabolomics.github.io/MetMashR/reference/filter_na.md)`(`\
`        column_name ``=`` ``"rt"`\
`    ``)`` ``+`\
`    `[`filter_na`](https://computational-metabolomics.github.io/MetMashR/reference/filter_na.md)`(`\
`        column_name ``=`` ``"median_ms2_scans"`\
`    ``)`` ``+`\
`    `[`filter_na`](https://computational-metabolomics.github.io/MetMashR/reference/filter_na.md)`(`\
`        column_name ``=`` ``"mzcloud_id"`\
`    ``)`` ``+`\
`    `[`filter_range`](https://computational-metabolomics.github.io/MetMashR/reference/filter_range.md)`(`\
`        column_name ``=`` ``"median_ms2_scans"``,`\
`        upper_limit ``=`` ``Inf``,`\
`        lower_limit ``=`` ``0``,`\
`        equal_to ``=`` ``TRUE`\
`    ``)`` ``+`\
`    `[`database_lookup`](https://computational-metabolomics.github.io/MetMashR/reference/database_lookup.md)`(`\
`        query_column ``=`` ``"hmdb_id"``,`\
`        database_column ``=`` ``"hmdb_id"``,`\
`        database ``=`` `[`rds_database`](https://computational-metabolomics.github.io/MetMashR/reference/rds_database.md)`(`` ``# stored copy of MTox700+`\
`            source ``=`` `[`system.file`](https://rdrr.io/r/base/system.file.html)`(`\
`                ``"extdata/vignette_snapshots/MTox700plus.rds"``,`\
`                package ``=`` ``"MetMashR"`\
`            ``)``,`\
`            .writable ``=`` ``FALSE`\
`        ``)``,`\
`        include ``=`` ``"inchikey"``,`\
`        suffix ``=`` ``""``,`\
`        not_found ``=`` ``NA`\
`    ``)`` ``+`\
`    `[`id_counts`](https://computational-metabolomics.github.io/MetMashR/reference/id_counts.md)`(`\
`        id_column ``=`` ``"inchikey"``,`\
`        count_column ``=`` ``"inchikey_count"``,`\
`        count_na ``=`` ``FALSE`\
`    ``)`\
`# apply`\
`SM`` ``<-`` `[`model_apply`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)`(``SM``, ``R``)`

The number of standard rows left after each step of the workflow:

\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`    step ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`        ``"imported"``, ``"rt not missing"``, ``"MS/MS scans not missing"``,`\
`        ``"mzCloud id not missing"``, ``"at least one MS/MS scan"`\
`    ``)``,`\
`    rows ``=`` `[`vapply`](https://rdrr.io/r/base/lapply.html)`(``1``:``5``, ``function``(``i``)`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(`[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``SM``[``i``]``)``$``data``)``, `[`numeric`](https://rdrr.io/r/base/numeric.html)`(``1``)``)`\
`)`\
`#>                      step rows`\
`#> 1                imported 1156`\
`#> 2          rt not missing  679`\
`#> 3 MS/MS scans not missing  658`\
`#> 4  mzCloud id not missing  463`\
`#> 5 at least one MS/MS scan  319`

After the filters, 319 rows remain, representing 64 distinct InChIKeys.

In this next plot we show the overlap in standards for each assay
detected by manual observation. The Venn diagram counts distinct
InChIKeys.

\
`C`` ``<-`` `[`annotation_venn_chart`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_venn_chart.md)`(`\
`    factor_name ``=`` ``"inchikey"``,`\
`    group_column ``=`` ``"ion_mode"``,`\
`    line_colour ``=`` ``"white"``,`\
`    fill_colour ``=`` ``".group"``,`\
`    legend ``=`` ``TRUE``,`\
`    labels ``=`` ``FALSE`\
`)`\
\
`## plot`\
[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``SM``)``)`

![Distinct InChIKeys of the filtered standards in each
assay.](annotate_mixtures_files/figure-html/annotate-mixtures-28-1.png)

Distinct InChIKeys of the filtered standards in each assay.

\

## Comparison of standards and annotations

Now that we have both the annotations and the standards we can look at
overlap between the identifiers in both sources, and begin to assess the
ability of the annotation software annotate the standards.

Here we plot a venn diagram showing the overlap in identifiers. As for
the previous charts, the Venn diagram counts distinct InChIKeys, not
annotation rows.

\
`# get processed data`\
`AN`` ``<-`` `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``FF``)`\
`AN``$``tag`` ``<-`` ``"Annotations"`\
`sM`` ``<-`` `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``SM``)`\
`sM``$``tag`` ``<-`` ``"Standards"`\
\
`# prepare chart`\
`C`` ``<-`` `[`annotation_venn_chart`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_venn_chart.md)`(`\
`    factor_name ``=`` ``"inchikey"``,`\
`    line_colour ``=`` ``"white"``,`\
`    fill_colour ``=`` ``".group"`\
`)`\
\
`# plot`\
[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, ``sM``, ``AN``)`` ``+`` `[`ggtitle`](https://ggplot2.tidyverse.org/reference/labs.html)`(``"All assays, all sources"``)`

![Distinct InChIKeys in the filtered standards and in the annotations
from all assays and
sources.](annotate_mixtures_files/figure-html/annotate-mixtures-29-1.png)

Distinct InChIKeys in the filtered standards and in the annotations from
all assays and sources.

These identifiers were not found in the filtered reference list. They
may reflect annotation errors, standards excluded by the filters or
differences in identifier matching. Investigate the unmatched records
before calling them false positives.

In the next plot we show similar Venn diagrams but for each assay
individually. These are followed by a 4-set Venn diagram that shows the
overlap between annotations from each assay that matched to a standard.

\
`G`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(``)`\
`VV`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(``)`\
`for`` ``(``k`` ``in`` `[`c`](https://rdrr.io/r/base/c.html)`(``"HILIC_NEG"``, ``"HILIC_POS"``, ``"LIPIDS_NEG"``, ``"LIPIDS_POS"``)``)`` ``{`\
`    ``wf`` ``<-`` `[`filter_labels`](https://computational-metabolomics.github.io/MetMashR/reference/filter_labels.md)`(`\
`        column_name ``=`` ``"assay"``,`\
`        labels ``=`` ``k``,`\
`        mode ``=`` ``"include"`\
`    ``)`\
`    ``wf1`` ``<-`` `[`model_apply`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)`(``wf``, ``AN``)`\
`    ``wf``$``column_name`` ``<-`` ``"ion_mode"`\
`    ``wf2`` ``<-`` `[`model_apply`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)`(``wf``, ``sM``)`\
`    ``G``[[``k``]``]`` ``<-`` `[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``wf2``)``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``wf1``)``)`\
\
`    ``V`` ``<-`` `[`filter_venn`](https://computational-metabolomics.github.io/MetMashR/reference/filter_venn.md)`(`\
`        factor_name ``=`` ``"inchikey"``,`\
`        tables ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``wf1``)``)``,`\
`        filter ``=`` `[`upset_min_groups`](https://computational-metabolomics.github.io/MetMashR/reference/upset_filters.md)`(``2``)``,`\
`        mode ``=`` ``"include"`\
`    ``)`\
`    ``V`` ``<-`` `[`model_apply`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)`(``V``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``wf2``)``)`\
`    ``VV``[[``k``]``]`` ``<-`` `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``V``)`\
`    ``VV``[[``k``]``]``$``tag`` ``<-`` ``k`\
`}`\
`r1`` ``<-`` ``cowplot``::`[`plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html)`(`\
`    plotlist ``=`` ``G``, nrow ``=`` ``2``,`\
`    labels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"HILIC_NEG"``, ``"HILIC_POS"``, ``"LIPIDS_NEG"``, ``"LIPIDS_POS"``)`\
`)`\
\
`cowplot``::`[`plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html)`(``r1``, `[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, ``VV``)``, nrow ``=`` ``2``, rel_heights ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``0.5``)``)`

![Distinct InChIKeys of the filtered standards and the annotations for
each assay (top), and the standards InChIKeys also annotated in each
assay
(bottom).](annotate_mixtures_files/figure-html/annotate-mixtures-30-1.png)

Distinct InChIKeys of the filtered standards and the annotations for
each assay (top), and the standards InChIKeys also annotated in each
assay (bottom).

\
`# distinct standard InChIKeys per assay, and how many were also annotated`\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`    assay ``=`` `[`names`](https://rdrr.io/r/base/names.html)`(``VV``)``,`\
`    standards ``=`` `[`vapply`](https://rdrr.io/r/base/lapply.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``VV``)``, ``function``(``k``)`` ``{`\
`        ``x`` ``<-`` ``sM``$``data``$``inchikey``[``sM``$``data``$``ion_mode`` ``==`` ``k``]`\
`        `[`length`](https://rdrr.io/r/base/length.html)`(`[`unique`](https://rdrr.io/r/base/unique.html)`(``x``[``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``x``)``]``)``)`\
`    ``}``, `[`numeric`](https://rdrr.io/r/base/numeric.html)`(``1``)``)``,`\
`    also_annotated ``=`` `[`vapply`](https://rdrr.io/r/base/lapply.html)`(``VV``, ``function``(``v``)`` ``{`\
`        `[`length`](https://rdrr.io/r/base/length.html)`(`[`unique`](https://rdrr.io/r/base/unique.html)`(``v``$``data``$``inchikey``[``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``v``$``data``$``inchikey``)``]``)``)`\
`    ``}``, `[`numeric`](https://rdrr.io/r/base/numeric.html)`(``1``)``)``,`\
`    row.names ``=`` ``NULL`\
`)`\
`#>        assay standards also_annotated`\
`#> 1  HILIC_NEG        59             26`\
`#> 2  HILIC_POS        57             21`\
`#> 3 LIPIDS_NEG        45             13`\
`#> 4 LIPIDS_POS        41              5`

The table counts distinct InChIKeys of the filtered standards for each
assay, and how many of them were also found among the annotations for
that assay. A shared InChIKey shows that the identifiers agree; it does
not on its own confirm that the annotated feature is the standard.

In the next plot we compare the overlap in InChIKey for each source.

\
`G`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(``)`\
`for`` ``(``k`` ``in`` `[`c`](https://rdrr.io/r/base/c.html)`(``"CD"``, ``"LS"``)``)`` ``{`\
`    ``wf`` ``<-`` `[`filter_labels`](https://computational-metabolomics.github.io/MetMashR/reference/filter_labels.md)`(`\
`        column_name ``=`` ``"source_name"``,`\
`        labels ``=`` ``k``,`\
`        mode ``=`` ``"include"`\
`    ``)`\
`    ``wf1`` ``<-`` `[`model_apply`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)`(``wf``, ``AN``)`\
\
`    ``G``[[``k``]``]`` ``<-`` `[`chart_plot`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)`(``C``, ``sM``, `[`predicted`](https://rdrr.io/pkg/struct/man/predicted.html)`(``wf1``)``)`\
`}`\
`cowplot``::`[`plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html)`(``plotlist ``=`` ``G``, nrow ``=`` ``1``, labels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"CD"``, ``"LS"``)``)`

![Distinct InChIKeys of the filtered standards and the annotations from
each
source.](annotate_mixtures_files/figure-html/annotate-mixtures-31-1.png)

Distinct InChIKeys of the filtered standards and the annotations from
each source.

\

## Session info

\
[`sessionInfo`](https://rdrr.io/r/utils/sessionInfo.html)`(``)`\
`#> R version 4.6.1 (2026-06-24)`\
`#> Platform: x86_64-pc-linux-gnu`\
`#> Running under: Ubuntu 24.04.5 LTS`\
`#> `\
`#> Matrix products: default`\
`#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 `\
`#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0`\
`#> `\
`#> locale:`\
`#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              `\
`#>  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    `\
`#>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   `\
`#>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 `\
`#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            `\
`#> [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       `\
`#> `\
`#> time zone: UTC`\
`#> tzcode source: system (glibc)`\
`#> `\
`#> attached base packages:`\
`#> [1] stats     graphics  grDevices utils     datasets  methods   base     `\
`#> `\
`#> other attached packages:`\
`#> [1] ggplotify_0.1.4      DT_0.34.0            dplyr_1.2.1         `\
`#> [4] structToolbox_1.25.0 ggplot2_4.0.3        MetMashR_1.7.3      `\
`#> [7] struct_1.25.0        BiocStyle_2.41.0    `\
`#> `\
`#> loaded via a namespace (and not attached):`\
`#>  [1] tidyselect_1.2.1            farver_2.1.2               `\
`#>  [3] S7_0.2.2                    fastmap_1.2.0              `\
`#>  [5] digest_0.6.39               lifecycle_1.0.5            `\
`#>  [7] magrittr_2.0.5              compiler_4.6.1             `\
`#>  [9] rlang_1.3.0                 sass_0.4.10                `\
`#> [11] tools_4.6.1                 yaml_2.3.12                `\
`#> [13] knitr_1.52                  S4Arrays_1.13.2            `\
`#> [15] labeling_0.4.3              htmlwidgets_1.6.4          `\
`#> [17] sp_2.2-4                    DelayedArray_0.39.8        `\
`#> [19] plyr_1.8.9                  xml2_1.6.0                 `\
`#> [21] RColorBrewer_1.1-3          aplot_0.3.2                `\
`#> [23] abind_1.4-8                 withr_3.0.3                `\
`#> [25] purrr_1.2.2                 BiocGenerics_0.59.12       `\
`#> [27] desc_1.4.3                  grid_4.6.1                 `\
`#> [29] stats4_4.6.1                scales_1.4.0               `\
`#> [31] SummarizedExperiment_1.43.0 cli_3.6.6                  `\
`#> [33] rmarkdown_2.32              ragg_1.5.2                 `\
`#> [35] generics_0.1.4              otel_0.2.0                 `\
`#> [37] httr_1.4.9                  cachem_1.1.0               `\
`#> [39] ggthemes_7.0.0              BiocManager_1.30.27        `\
`#> [41] XVector_0.53.0              matrixStats_1.5.0          `\
`#> [43] vctrs_0.7.3                 yulab.utils_0.2.5          `\
`#> [45] Matrix_1.7-6                jsonlite_2.0.0             `\
`#> [47] bookdown_0.48               patchwork_1.3.2            `\
`#> [49] gridGraphics_0.5-1          IRanges_2.47.5             `\
`#> [51] S4Vectors_0.51.10           systemfonts_1.3.2          `\
`#> [53] crosstalk_1.2.2             jquerylib_0.1.4            `\
`#> [55] tidyr_1.3.2                 glue_1.8.1                 `\
`#> [57] ggVennDiagram_1.5.7         pkgdown_2.2.1.9000         `\
`#> [59] cowplot_1.2.0               stringi_1.8.9              `\
`#> [61] gtable_0.3.6                GenomicRanges_1.65.4       `\
`#> [63] tibble_3.3.1                pillar_1.11.1              `\
`#> [65] rappdirs_0.3.4              htmltools_0.5.9            `\
`#> [67] Seqinfo_1.3.2               R6_2.6.1                   `\
`#> [69] httr2_1.3.0                 textshaping_1.0.5          `\
`#> [71] evaluate_1.0.5              lattice_0.23-1             `\
`#> [73] Biobase_2.73.2              openxlsx_4.2.9             `\
`#> [75] ggfun_0.2.2                 bslib_0.12.0               `\
`#> [77] Rcpp_1.1.2                  zip_3.0.2                  `\
`#> [79] gridExtra_2.3.1             SparseArray_1.13.4         `\
`#> [81] xfun_0.62                   fs_2.1.0                   `\
`#> [83] MatrixGenerics_1.25.0       forcats_1.0.1              `\
`#> [85] pkgconfig_2.0.3`
