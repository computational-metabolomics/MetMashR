# Combine annotation sources (tables)

Annotation tables are joined and matching columns merged.

## Usage

``` r
combine_sources(
  source_list,
  matching_columns = NULL,
  keep_cols = NULL,
  source_col = "annotation_source",
  exclude_cols = NULL,
  tag = "combined",
  as = annotation_source(name = "combined", description =
    paste0("A source created by combining two or ", "more sources")),
  ...
)
```

## Arguments

- source_list:

  (list) A list of annotation sources to be combined.

- matching_columns:

  (character, NULL) A named vector of columns names to be created by
  merging columns from individual sources. e.g. `c('world'='hello')`
  will rename the 'hello' column to 'world' if found in any of the
  tables. The default is `NULL`.

- keep_cols:

  (character, NULL) A list of column names to keep in the combined table
  (padded with NA) if detected in one of the input tables. Special case
  ".all" will keep all columns from all tables. The default is `NULL`.

- source_col:

  (character) The column name that will be created to contain a tag to
  indicate which source the annotation originated from. The default is
  `"annotation_source"`.

- exclude_cols:

  (NULL, character) Column names to be excluded from the merged
  annotation table. Note this is applied after `keep_cols`. The default
  is `NULL`.

- tag:

  (character) The tag given to the newly combined table. The default is
  `"combined"`.

- as:

  (annotation_source) An annotation_source object to use as the base
  class for the combined sources. The default is
  `annotation_source(name = "combined", description = paste0("A source created by combining two or ", "more sources"))`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `combine_sources` object with the following `output` slots:

|  |  |
|----|----|
| `combined_table` | (annotation_source) The annotation tabel after combining the input tables. |

## Inheritance

A `combine_sources` object inherits the following `struct` classes:\
\
`[combine_sources]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- combine_sources(
        source_list = list(),
        matching_columns = NULL,
        keep_cols = NULL,
        source_col = "annotation_source",
        exclude_cols = NULL,
        tag = "combined",
        as = annotation_source())
```
