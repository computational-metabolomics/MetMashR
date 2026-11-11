# MetMashR v1.7.3
* add `zenodo_file` to import files from Zenodo records
* vignettes now build without network access: online resources are read from
  stored copies in `inst/extdata/vignette_snapshots` and all lookups use
  `cache_mode = "offline"`; `inst/scripts/make_vignette_snapshots.R`
  rebuilds the stored copies and refreshes the lookup caches
* revise all vignettes following review: new titles, clearer explanations,
  worked examples, and counts computed in the vignettes
* `pubchem_property_lookup` replaces the deprecated PubChem property names
  `CanonicalSMILES` and `IsomericSMILES` with `ConnectivitySMILES` and
  `SMILES`, with a warning
* `kegg_lookup` caches ids with no match as NA so they are not queried again
* `rest_api` lookups no longer rewrite the cache when nothing was queried
* document `model_train` and `model_predict` with `model_apply`
* fix `prioritise_columns` assigning the last source tag to rows where all
  columns are NA; the source is now NA
* fix `ls_source` truncating `LipidName` at `/`, `+` or `-` inside brackets
* fix `mwb_refmet_database` failing when the cache has duplicate entries for
  the RefMet download

# MetMashR v1.7.2
* add `pubchem_id_exchange` for batch identifier conversion
* add `classyfire_batch_lookup` for batch ClassyFire queries
* add `cts_lite_lookup` for Chemical Translation Service queries
* add `mwb_refmet_lookup` and `mwb_study_source` for Metabolomics Workbench
* add `lipidmaps_database` to import the full LIPID MAPS database
* add `columns` input to `chebi_lookup`
* add `cache_mode` input to `rest_api` lookups and `kegg_lookup`
* add `unique` input to `select_max` and `select_min`
* add case study vignette reimplementing the Metabolites Merging Strategy
* vectorise `mz_match` and `rt_match` interval-overlap matching
* `mspurity_source` is now an `lcms_table`
* fix `vertical_join` matching_columns rename direction
* fix silent type coercion in `database_lookup`
* fix crash when importing an empty `ls_source`
* fix outside-label positioning in bar and pie charts
* fix `BiocFileCache_database` when the cache has duplicate entries
* use environment variable for  GitHub API requests to avoid rate limits

# MetMashR v1.7.1
* add ChEBI lookup class

# MetMashR v1.5.1
* Rebuild documentation
* move cowplot to Suggests
* add ggplot2 to namespace imports

# MetMashR v1.3.3
* Change interface to annotation_upset
* Use ggVennDiagram instead of ComplexUpset and RVenn
* Re-use upset code for filter_venn
* Remove openbabel_structure and ChemmineOB dependency
* Set bg colour of annotation_barchart to white

# MetMashR v0.99.0
* Preparation for Bioc submission

# MetMashR v0.1.0
* Initial commit.
