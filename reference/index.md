# Package index

## Annotation sources

An `annotation source` is the dataset object used by all MetMashR
objects. Different types of source have been defined, depending on the
intended use of the data.

- [`CompoundDb_source()`](https://computational-metabolomics.github.io/MetMashR/reference/CompoundDb_source.md)
  : Import CompDB source
- [`annotation_source()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_source.md)
  : An annotation source
- [`cd_source()`](https://computational-metabolomics.github.io/MetMashR/reference/cd_source.md)
  : LCMS table
- [`import_source()`](https://computational-metabolomics.github.io/MetMashR/reference/import_source.md)
  : Import_source
- [`ls_source()`](https://computational-metabolomics.github.io/MetMashR/reference/ls_source.md)
  : LCMS table
- [`mspurity_source()`](https://computational-metabolomics.github.io/MetMashR/reference/mspurity_source.md)
  : msPurity source
- [`mwb_study_source()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_study_source.md)
  : Metabolomics Workbench study source
- [`read_source()`](https://computational-metabolomics.github.io/MetMashR/reference/read_source.md)
  : Import annotation source

## Annotation tables

Annotation tables represent the data imported from a source that
includes experimentally measured data. Annotation tables are extended to
support the specifics of an analytical platform used to collect the
data.

- [`annotation_table()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_table.md)
  : An annotation table
- [`lcms_table()`](https://computational-metabolomics.github.io/MetMashR/reference/lcms_table.md)
  : LCMS table

## Annotation databases

Annotation databases are (often remote) sources of annotation related
meta data, such as molecular identifiers, pathways etc.

- [`AnnotationDb_database()`](https://computational-metabolomics.github.io/MetMashR/reference/AnnotationDb_database.md)
  : AnnotationDb database
- [`BiocFileCache_database()`](https://computational-metabolomics.github.io/MetMashR/reference/BiocFileCache_database.md)
  : Cached database
- [`GO_database()`](https://computational-metabolomics.github.io/MetMashR/reference/GO_database.md)
  : GO.db
- [`MTox700plus_database()`](https://computational-metabolomics.github.io/MetMashR/reference/MTox700plus_database.md)
  : MTox700plus_database
- [`PathBank_metabolite_database()`](https://computational-metabolomics.github.io/MetMashR/reference/PathBank_metabolite_database.md)
  : PathBank_metabolite_database
- [`annotation_database()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_database.md)
  : An annotation database
- [`excel_database()`](https://computational-metabolomics.github.io/MetMashR/reference/excel_database.md)
  : Excel database
- [`lipidmaps_database()`](https://computational-metabolomics.github.io/MetMashR/reference/lipidmaps_database.md)
  : LIPID MAPS database
- [`mwb_refmet_database()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_refmet_database.md)
  : mwb_refmet_database
- [`rdata_database()`](https://computational-metabolomics.github.io/MetMashR/reference/rdata_database.md)
  : rdata database
- [`rds_database()`](https://computational-metabolomics.github.io/MetMashR/reference/rds_database.md)
  : rds database
- [`read_database()`](https://computational-metabolomics.github.io/MetMashR/reference/read_database.md)
  : Read a database
- [`sqlite_database()`](https://computational-metabolomics.github.io/MetMashR/reference/sqlite_database.md)
  : SQLite database
- [`write_database()`](https://computational-metabolomics.github.io/MetMashR/reference/write_database.md)
  : Write to a database
- [`rds_cache()`](https://computational-metabolomics.github.io/MetMashR/reference/rds_cache.md)
  : rds cache
- [`github_file()`](https://computational-metabolomics.github.io/MetMashR/reference/github_file.md)
  : GitHub file
- [`zenodo_file()`](https://computational-metabolomics.github.io/MetMashR/reference/zenodo_file.md)
  : Zenodo file
- [`is_writable()`](https://computational-metabolomics.github.io/MetMashR/reference/is_writable.md)
  : Is database writable

## REST API interfaces

MetMashR includes a REST API object that has been extended to accomodate
various services.

- [`rest_api()`](https://computational-metabolomics.github.io/MetMashR/reference/rest_api.md)
  : rest_api
- [`chebi_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/chebi_lookup.md)
  : ID/synonym lookup via ChEBI
- [`classyfire_batch_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/classyfire_batch_lookup.md)
  : ClassyFire batch lookup
- [`classyfire_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/classyfire_lookup.md)
  : Query ClassyFire database
- [`cts_lite_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/cts_lite_lookup.md)
  : Batch lookup via CTS-Lite
- [`database_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/database_lookup.md)
  : ID lookup by database
- [`eutils_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/eutils_lookup.md)
  : NCBI E-utils query
- [`hmdb_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/hmdb_lookup.md)
  : Compound ID lookup via pubchem
- [`kegg_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/kegg_lookup.md)
  : Convert to or from kegg identifiers
- [`lipidmaps_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/lipidmaps_lookup.md)
  : LipidMaps api lookup
- [`mwb_compound_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_compound_lookup.md)
  : Convert to/from kegg identifiers
- [`mwb_refmet_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_refmet_lookup.md)
  : Metabolomics Workbench RefMet lookup
- [`opsin_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/opsin_lookup.md)
  : Compound ID lookup via OPSIN
- [`pubchem_compound_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/pubchem_compound_lookup.md)
  : Compound ID lookup via PubChem
- [`pubchem_property_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/pubchem_property_lookup.md)
  : Compound property lookup via pubchem
- [`pubchem_id_exchange()`](https://computational-metabolomics.github.io/MetMashR/reference/pubchem_id_exchange.md)
  : PubChem ID Exchange via PUG

## Annotation table mashing

These models provide steps for cleaning, filtering, prioritising and
combining annotation tables from different sources.

- [`add_columns()`](https://computational-metabolomics.github.io/MetMashR/reference/add_columns.md)
  : Add columns
- [`add_labels()`](https://computational-metabolomics.github.io/MetMashR/reference/add_labels.md)
  : Add column of labels
- [`calc_ppm_diff()`](https://computational-metabolomics.github.io/MetMashR/reference/calc_ppm_diff.md)
  : Calculate ppm difference
- [`calc_rt_diff()`](https://computational-metabolomics.github.io/MetMashR/reference/calc_rt_diff.md)
  : Calculate RT difference
- [`prioritise_columns()`](https://computational-metabolomics.github.io/MetMashR/reference/prioritise_columns.md)
  : Combine several columns into a single column.
- [`combine_sources()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_sources.md)
  : Combine annotation sources (tables)
- [`vertical_join()`](https://computational-metabolomics.github.io/MetMashR/reference/vertical_join.md)
  : Join sources vertically
- [`combine_records()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records.md)
  : Combine annotation records (rows)
- [`filter_labels()`](https://computational-metabolomics.github.io/MetMashR/reference/filter_labels.md)
  : Filter by factor labels
- [`filter_range()`](https://computational-metabolomics.github.io/MetMashR/reference/filter_range.md)
  : Filter by range
- [`filter_na()`](https://computational-metabolomics.github.io/MetMashR/reference/filter_na.md)
  : Filter by missing values
- [`filter_venn()`](https://computational-metabolomics.github.io/MetMashR/reference/filter_venn.md)
  : Filter by factor levels
- [`filter_records()`](https://computational-metabolomics.github.io/MetMashR/reference/filter_records.md)
  : Filter rows
- [`id_counts()`](https://computational-metabolomics.github.io/MetMashR/reference/id_counts.md)
  : id counts
- [`mz_match()`](https://computational-metabolomics.github.io/MetMashR/reference/mz_match.md)
  : mz matching
- [`rt_match()`](https://computational-metabolomics.github.io/MetMashR/reference/rt_match.md)
  : rt matching
- [`mzrt_match()`](https://computational-metabolomics.github.io/MetMashR/reference/mzrt_match.md)
  : mz matching
- [`normalise_lipids()`](https://computational-metabolomics.github.io/MetMashR/reference/normalise_lipids.md)
  : Normalise Lipids nomenclature
- [`normalise_strings()`](https://computational-metabolomics.github.io/MetMashR/reference/normalise_strings.md)
  : Normalise string
- [`trim_whitespace()`](https://computational-metabolomics.github.io/MetMashR/reference/trim_whitespace.md)
  : Trim whitespace
- [`combine_columns()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_columns.md)
  : Combine columns
- [`select_columns()`](https://computational-metabolomics.github.io/MetMashR/reference/select_columns.md)
  : Select columns
- [`remove_columns()`](https://computational-metabolomics.github.io/MetMashR/reference/remove_columns.md)
  : Select columns
- [`split_column()`](https://computational-metabolomics.github.io/MetMashR/reference/split_column.md)
  : Split a column
- [`split_records()`](https://computational-metabolomics.github.io/MetMashR/reference/split_records.md)
  : Expand records
- [`AnnotationDb_select()`](https://computational-metabolomics.github.io/MetMashR/reference/AnnotationDb_select.md)
  : Select columns from AnnotationDb database
- [`compute_column()`](https://computational-metabolomics.github.io/MetMashR/reference/compute_column.md)
  : Compute a column
- [`compute_record()`](https://computational-metabolomics.github.io/MetMashR/reference/compute_record.md)
  : Compute a value for a record
- [`pivot_columns()`](https://computational-metabolomics.github.io/MetMashR/reference/pivot_columns.md)
  : Pivot longer
- [`rename_columns()`](https://computational-metabolomics.github.io/MetMashR/reference/rename_columns.md)
  : Select columns
- [`unique_records()`](https://computational-metabolomics.github.io/MetMashR/reference/unique_records.md)
  : Keep unique_records

## Combining records

Combining records is an important step in the annotation mashing
workflow. These functions can be used with the combine_records function
to merge the information for multiple records in different ways.

- [`compute_mode()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records_helper_functions.md)
  [`compute_mean()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records_helper_functions.md)
  [`compute_median()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records_helper_functions.md)
  [`fuse()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records_helper_functions.md)
  [`select_max()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records_helper_functions.md)
  [`select_min()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records_helper_functions.md)
  [`select_match()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records_helper_functions.md)
  [`select_exact()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records_helper_functions.md)
  [`fuse_unique()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records_helper_functions.md)
  [`prioritise()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records_helper_functions.md)
  [`nothing()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records_helper_functions.md)
  [`count_records()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records_helper_functions.md)
  [`select_grade()`](https://computational-metabolomics.github.io/MetMashR/reference/combine_records_helper_functions.md)
  : Combine records helper functions

## Charts

Chart objects are wrappers around ggplot objects plots useful for
exploring and visualising the information present in a table of
annotations.

- [`mwb_structure()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_structure.md)
  : MWB molecular structure
- [`pubchem_structure()`](https://computational-metabolomics.github.io/MetMashR/reference/pubchem_structure.md)
  : PubChem molecular structure
- [`annotation_bar_chart()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_bar_chart.md)
  : Annotation bar chart
- [`annotation_pie_chart()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_pie_chart.md)
  : Annotation pie chart
- [`annotation_upset_chart()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_upset_chart.md)
  : Annotation UpSet chart
- [`annotation_venn_chart()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_venn_chart.md)
  : Annotation venn chart
- [`annotation_histogram()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_histogram.md)
  : Annotation histogram
- [`annotation_histogram2d()`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_histogram2d.md)
  : Annotation 2D histogram
- [`pubchem_widget()`](https://computational-metabolomics.github.io/MetMashR/reference/pubchem_widget.md)
  : PubChem widget
- [`upset_min_size()`](https://computational-metabolomics.github.io/MetMashR/reference/upset_filters.md)
  [`upset_min_groups()`](https://computational-metabolomics.github.io/MetMashR/reference/upset_filters.md)
  [`upset_max_groups()`](https://computational-metabolomics.github.io/MetMashR/reference/upset_filters.md)
  [`upset_intersections()`](https://computational-metabolomics.github.io/MetMashR/reference/upset_filters.md)
  : UpSet chart filter helper functions

## BiocFileCache_database helper functions

These functions can be used with BiocFileCache_database objects to
modify a dowloaded resource before caching, or to parse the downloaded
resource when retrieved from the cache.

- [`cache_as_is()`](https://computational-metabolomics.github.io/MetMashR/reference/cache_as_is.md)
  : Cache file with no changes using BiocFileCache
- [`unzip_before_cache()`](https://computational-metabolomics.github.io/MetMashR/reference/unzip_before_cache.md)
  : Unzip file before caching with BiocFileCache_database

## Dictionaries for normalising strings

These lists define patterns and replacements to match when using the
normalise_strings object.

- [`greek_dictionary`](https://computational-metabolomics.github.io/MetMashR/reference/greek_dictionary.md)
  : Greek dictionary
- [`racemic_dictionary`](https://computational-metabolomics.github.io/MetMashR/reference/racemic_dictionary.md)
  : Racemic dictionary
- [`tripeptide_dictionary`](https://computational-metabolomics.github.io/MetMashR/reference/tripeptide_dictionary.md)
  : Tripeptide dictionary

## Additional functions

Supporting functions are used by MetMashR

- [`check_for_columns()`](https://computational-metabolomics.github.io/MetMashR/reference/check_for_columns.md)
  :

  Check for columns in an `annotation_source`

- [`required_cols()`](https://computational-metabolomics.github.io/MetMashR/reference/required_cols.md)
  : Required columns in an annotation source

- [`wherever()`](https://computational-metabolomics.github.io/MetMashR/reference/wherever.md)
  : Filter helper function to select records

- [`chart_plot()`](https://computational-metabolomics.github.io/MetMashR/reference/chart_plot.md)
  : chart_plot method

- [`model_apply()`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)
  [`model_train()`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)
  [`model_predict()`](https://computational-metabolomics.github.io/MetMashR/reference/model_apply.md)
  : Apply method
