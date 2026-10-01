# Expand records

Expand single records into multiple records by splitting strings in a
named column at the chosen separator. For example, if a for a record the
column `synonyms = c("glucose,dextrose")` then by splitting at the comma
results in two records, one for glucose and one for dextrose with
identical values (apart from the column being split). The original
record is removed.

## Usage

``` r
split_records(column_name, separator, clean = TRUE, ...)
```

## Arguments

- column_name:

  (character) The column name of the `annotation_source` to split
  intomultiple records.

- separator:

  (character) The substring used to split the values in column_name into
  multiple records.

- clean:

  (logical) Remove the original column. If FALSE the original column
  will be retained in the final output with .original appended to the
  column name. The default is `TRUE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `split_records` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The updated annotations as an `annotation_source` object. |

## Details

This object makes use of functionality from the following packages:

- `tidytext`

## Inheritance

A `split_records` object inherits the following `struct` classes:\
\
`[split_records]` -\> `[model]` -\> `[struct_class]`

## References

Silge J, Robinson D (2016). "tidytext: Text Mining and Analysis Using
Tidy Data Principles in R." *JOSS*, *1*(3). doi:10.21105/joss.00037
<https://doi.org/10.21105/joss.00037>.
<http://dx.doi.org/10.21105/joss.00037>.

## Examples

``` r
M <- split_records(
        column_name = character(0),
        separator = ",",
        clean = FALSE)
```
