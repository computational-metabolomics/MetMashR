# Filter by factor labels

Removes (or includes) annotations such that the named column excludes
(or includes) the specified labels.

## Usage

``` r
filter_labels(
  column_name,
  labels,
  mode = "exclude",
  perl = FALSE,
  fixed = FALSE,
  match_na = FALSE,
  ...
)
```

## Arguments

- column_name:

  (character) The column name to filter.

- labels:

  (character) The labels to filter by. Uses `[grepl()]` so regex is
  accepted e.g. for partial matching or labels.

- mode:

  (character) Filter mode. Allowed values are limited to the following:

  - `"exclude"`: The specified labels are removed from the annotation
    table.

  - `"include"`: Only the specified labels are retained in the
    annotation table.

  The default is `"exclude"`.

- perl:

  (logical) Use a Perl-compatible regex. The default is `FALSE`.\

- fixed:

  (logical) Use exact matching. The default is `FALSE`.\

- match_na:

  (logical) Match NA. Allowed values are limited to the following:

  - `"TRUE"`: NA values will be treated as if they matched to one of the
    labels.

  - `"FALSE"`: NA values will be treated as though they did not match to
    any of the labels.

  The default is `FALSE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `filter_labels` object with the following `output` slots:

|  |  |
|----|----|
| `filtered` | (annotation_source) The annotation_source after filtering. |
| `flags` | (data.frame) A list of flags indicating which annotations had a matching label. |

## Inheritance

A `filter_labels` object inherits the following `struct` classes:\
\
`[filter_labels]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- filter_labels(
        column_name = "V1",
        labels = "",
        mode = "exclude",
        perl = FALSE,
        fixed = FALSE,
        match_na = FALSE)
```
