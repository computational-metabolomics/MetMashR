# Filter by factor levels

Removes (or includes) annotations such that the named column excludes
(or includes) the specified intersection levels. Supports any number of
groups using intersection-based filtering. If no levels are specified,
all available intersection levels will be returned for inspection. If
invalid levels are specified, a warning will be shown with the list of
valid levels.

## Usage

``` r
filter_venn(
  factor_name,
  group_column = NULL,
  tables = NULL,
  filter = NULL,
  mode = "include",
  ...
)
```

## Arguments

- factor_name:

  (character) The name of the column(s) in the `annotation_source` to
  generate intersection groups from. Supports any number of columns for
  intersection-based filtering.

- group_column:

  (character, NULL) The name of the column in the `annotation_source` to
  create groups from in the Venn diagram. This parameter is ignored if
  `!is.null(tables)`, as each table is considered to be a group. This
  parameter is also ignored if more than one `factor_name` is provided,
  as each column is considered a group. The default is `NULL`.

- tables:

  (list, NULL) A list of `annotation_sources` to generate the venn
  groups from. If the only table of interest is the table coming in from
  `model_apply` then set `tables = NULL` and use `group_column`. The
  default is `NULL`.

- filter:

  (function, NULL) A function to filter intersections based on their
  properties. The function should take region_data as input and return a
  logical vector indicating which intersections to keep. Use
  upset_intersections(), upset_min_size(), upset_min_groups(),
  upset_max_groups(), or create custom filter functions. The default is
  `NULL`.

- mode:

  (character) Filter mode. Allowed values are limited to the following:

  - `"include"`: Only items that appear in the filtered intersections
    are kept in the output.

  - `"exclude"`: Items that appear in the filtered intersections are
    removed from the output.

  The default is `"include"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `filter_venn` object with the following `output` slots:

|  |  |
|----|----|
| `filtered` | (annotation_source) Annotation_source after filtering. |
| `flags` | (data.frame) A list of flags indicating which annotations were removed. |

## Inheritance

A `filter_venn` object inherits the following `struct` classes:\
\
`[filter_venn]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- filter_venn(
        factor_name = "V1",
        group_column = NULL,
        tables = NULL,
        filter = NULL,
        mode = "include")
```
