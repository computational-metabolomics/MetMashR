# Apply method

Applies method to the input DatasetExperiment

## Usage

``` r
# S4 method for class 'model,annotation_source'
model_apply(M, D)

# S4 method for class 'model,list'
model_apply(M, D)

# S4 method for class 'model_seq,list'
model_apply(M, D)

# S4 method for class 'model_seq,annotation_source'
model_apply(M, D)

# S4 method for class 'AnnotationDb_select,annotation_source'
model_apply(M, D)

# S4 method for class 'CompoundDb_source,annotation_source'
model_apply(M, D)

# S4 method for class 'add_columns,annotation_source'
model_apply(M, D)

# S4 method for class 'add_labels,annotation_source'
model_apply(M, D)

# S4 method for class 'calc_ppm_diff,annotation_table'
model_apply(M, D)

# S4 method for class 'calc_rt_diff,annotation_table'
model_apply(M, D)

# S4 method for class 'rest_api,annotation_source'
model_apply(M, D)

# S4 method for class 'chebi_lookup,annotation_source'
model_apply(M, D)

# S4 method for class 'combine_columns,annotation_source'
model_apply(M, D)

# S4 method for class 'combine_records,annotation_source'
model_apply(M, D)

# S4 method for class 'combine_sources,annotation_source'
model_apply(M, D)

# S4 method for class 'combine_sources,list'
model_apply(M, D)

# S4 method for class 'compute_column,annotation_source'
model_apply(M, D)

# S4 method for class 'compute_record,annotation_source'
model_apply(M, D)

# S4 method for class 'cts_lite_lookup,annotation_source'
model_apply(M, D)

# S4 method for class 'database_lookup,annotation_source'
model_apply(M, D)

# S4 method for class 'split_records,annotation_source'
model_apply(M, D)

# S4 method for class 'filter_labels,annotation_source'
model_apply(M, D)

# S4 method for class 'filter_na,annotation_source'
model_apply(M, D)

# S4 method for class 'filter_range,annotation_source'
model_apply(M, D)

# S4 method for class 'filter_records,annotation_source'
model_apply(M, D)

# S4 method for class 'filter_venn,annotation_source'
model_apply(M, D)

# S4 method for class 'id_counts,annotation_source'
model_apply(M, D)

# S4 method for class 'import_source,annotation_source'
model_apply(M, D)

# S4 method for class 'kegg_lookup,annotation_source'
model_apply(M, D)

# S4 method for class 'mz_match,annotation_source'
model_apply(M, D)

# S4 method for class 'mzrt_match,lcms_table'
model_apply(M, D)

# S4 method for class 'normalise_lipids,annotation_source'
model_apply(M, D)

# S4 method for class 'normalise_strings,annotation_source'
model_apply(M, D)

# S4 method for class 'pivot_columns,annotation_source'
model_apply(M, D)

# S4 method for class 'prioritise_columns,annotation_source'
model_apply(M, D)

# S4 method for class 'remove_columns,annotation_source'
model_apply(M, D)

# S4 method for class 'rename_columns,annotation_source'
model_apply(M, D)

# S4 method for class 'rt_match,annotation_table'
model_apply(M, D)

# S4 method for class 'select_columns,annotation_source'
model_apply(M, D)

# S4 method for class 'split_column,annotation_source'
model_apply(M, D)

# S4 method for class 'trim_whitespace,annotation_source'
model_apply(M, D)

# S4 method for class 'unique_records,annotation_source'
model_apply(M, D)
```

## Arguments

- M:

  a method object

- D:

  another object used by the first

## Value

Returns a modified method object

## Examples

``` r
M <- example_model()
M <- model_apply(M, iris_DatasetExperiment())
```
