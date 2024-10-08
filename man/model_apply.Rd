% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotation_source_class.R,
%   R/AnnotationDb_select_class.R, R/CompoundDb_source_class.R,
%   R/add_columns_class.R, R/add_labels_class.R, R/calc_ppm_diff_class.R,
%   R/calc_rt_diff_class.R, R/rest_api_class.R, R/combine_columns_class.R,
%   R/combine_records_class.R, R/combine_sources.R, R/compute_column_class.R,
%   R/compute_record_class.R, R/database_lookup_class.R,
%   R/expand_records_class.R, R/filter_labels_class.R,
%   R/filter_na_class.R, R/filter_range_class.R, R/filter_records_class.R,
%   R/filter_venn_class.R, R/id_count_class.R, R/import_source_class.R,
%   R/kegg_lookup_class.R, R/model_apply_doc.R, R/mspurity_source_class.R,
%   R/mz_match_class.R, R/mzrt_match_class.R, R/normalise_lipids_class.R,
%   R/normalise_strings_class.R, R/pivot_columns_class.R,
%   R/prioritise_columns_class.R, R/remove_columns_class.R,
%   R/rename_columns_class.R, R/rt_match_class.R, R/select_columns_class.R,
%   R/split_column_class.R, R/trim_whitespace_class.R, R/unique_records_class.R
\name{model_apply,model,annotation_source-method}
\alias{model_apply,model,annotation_source-method}
\alias{model_apply,model,list-method}
\alias{model_apply,model_seq,list-method}
\alias{model_apply,model_seq,annotation_source-method}
\alias{model_apply,AnnotationDb_select,annotation_source-method}
\alias{model_apply,CompoundDb_source,annotation_source-method}
\alias{model_apply,add_columns,annotation_source-method}
\alias{model_apply,add_labels,annotation_source-method}
\alias{model_apply,calc_ppm_diff,annotation_table-method}
\alias{model_apply,calc_rt_diff,annotation_table-method}
\alias{model_apply,rest_api,annotation_source-method}
\alias{model_apply,combine_columns,annotation_source-method}
\alias{model_apply,combine_records,annotation_source-method}
\alias{model_apply,combine_sources,annotation_source-method}
\alias{model_apply,combine_sources,list-method}
\alias{model_apply,compute_column,annotation_source-method}
\alias{model_apply,compute_record,annotation_source-method}
\alias{model_apply,database_lookup,annotation_source-method}
\alias{model_apply,split_records,annotation_source-method}
\alias{model_apply,filter_labels,annotation_source-method}
\alias{model_apply,filter_na,annotation_source-method}
\alias{model_apply,filter_range,annotation_source-method}
\alias{model_apply,filter_records,annotation_source-method}
\alias{model_apply,filter_venn,annotation_source-method}
\alias{model_apply,id_counts,annotation_source-method}
\alias{model_apply,import_source,annotation_source-method}
\alias{model_apply,kegg_lookup,annotation_source-method}
\alias{model_apply}
\alias{model_apply,mspurity_source,lcms_table-method}
\alias{model_apply,mz_match,annotation_source-method}
\alias{model_apply,mzrt_match,lcms_table-method}
\alias{model_apply,normalise_lipids,annotation_source-method}
\alias{model_apply,normalise_strings,annotation_source-method}
\alias{model_apply,pivot_columns,annotation_source-method}
\alias{model_apply,prioritise_columns,annotation_source-method}
\alias{model_apply,remove_columns,annotation_source-method}
\alias{model_apply,rename_columns,annotation_source-method}
\alias{model_apply,rt_match,annotation_table-method}
\alias{model_apply,select_columns,annotation_source-method}
\alias{model_apply,split_column,annotation_source-method}
\alias{model_apply,trim_whitespace,annotation_source-method}
\alias{model_apply,unique_records,annotation_source-method}
\title{Apply method}
\usage{
\S4method{model_apply}{model,annotation_source}(M, D)

\S4method{model_apply}{model,list}(M, D)

\S4method{model_apply}{model_seq,list}(M, D)

\S4method{model_apply}{model_seq,annotation_source}(M, D)

\S4method{model_apply}{AnnotationDb_select,annotation_source}(M, D)

\S4method{model_apply}{CompoundDb_source,annotation_source}(M, D)

\S4method{model_apply}{add_columns,annotation_source}(M, D)

\S4method{model_apply}{add_labels,annotation_source}(M, D)

\S4method{model_apply}{calc_ppm_diff,annotation_table}(M, D)

\S4method{model_apply}{calc_rt_diff,annotation_table}(M, D)

\S4method{model_apply}{rest_api,annotation_source}(M, D)

\S4method{model_apply}{combine_columns,annotation_source}(M, D)

\S4method{model_apply}{combine_records,annotation_source}(M, D)

\S4method{model_apply}{combine_sources,annotation_source}(M, D)

\S4method{model_apply}{combine_sources,list}(M, D)

\S4method{model_apply}{compute_column,annotation_source}(M, D)

\S4method{model_apply}{compute_record,annotation_source}(M, D)

\S4method{model_apply}{database_lookup,annotation_source}(M, D)

\S4method{model_apply}{split_records,annotation_source}(M, D)

\S4method{model_apply}{filter_labels,annotation_source}(M, D)

\S4method{model_apply}{filter_na,annotation_source}(M, D)

\S4method{model_apply}{filter_range,annotation_source}(M, D)

\S4method{model_apply}{filter_records,annotation_source}(M, D)

\S4method{model_apply}{filter_venn,annotation_source}(M, D)

\S4method{model_apply}{id_counts,annotation_source}(M, D)

\S4method{model_apply}{import_source,annotation_source}(M, D)

\S4method{model_apply}{kegg_lookup,annotation_source}(M, D)

\S4method{model_apply}{mspurity_source,lcms_table}(M, D)

\S4method{model_apply}{mz_match,annotation_source}(M, D)

\S4method{model_apply}{mzrt_match,lcms_table}(M, D)

\S4method{model_apply}{normalise_lipids,annotation_source}(M, D)

\S4method{model_apply}{normalise_strings,annotation_source}(M, D)

\S4method{model_apply}{pivot_columns,annotation_source}(M, D)

\S4method{model_apply}{prioritise_columns,annotation_source}(M, D)

\S4method{model_apply}{remove_columns,annotation_source}(M, D)

\S4method{model_apply}{rename_columns,annotation_source}(M, D)

\S4method{model_apply}{rt_match,annotation_table}(M, D)

\S4method{model_apply}{select_columns,annotation_source}(M, D)

\S4method{model_apply}{split_column,annotation_source}(M, D)

\S4method{model_apply}{trim_whitespace,annotation_source}(M, D)

\S4method{model_apply}{unique_records,annotation_source}(M, D)
}
\arguments{
\item{M}{a method object}

\item{D}{another object used by the first}
}
\value{
Returns a modified method object
}
\description{
Applies method to the input DatasetExperiment
}
\examples{
M = example_model()
M = model_apply(M,iris_DatasetExperiment())
}
