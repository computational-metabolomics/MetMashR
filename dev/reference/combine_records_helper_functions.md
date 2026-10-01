# Combine records helper functions

This page documents helper functions for use with
[`combine_records()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/combine_records.md).

## Usage

``` r
compute_mode(ties = FALSE, na.rm = TRUE)

compute_mean(na.rm = TRUE)

compute_median(na.rm = TRUE)

fuse(separator, na_string = "NA")

select_max(max_col, use_abs = FALSE, keep_NA = FALSE, unique = FALSE)

select_min(min_col, use_abs = FALSE, keep_NA = FALSE, unique = FALSE)

select_match(match_col, search_col, separator, na_string = "NA")

select_exact(match_col, match, separator, na_string = "NA")

fuse_unique(
  separator,
  na_string = "NA",
  digits = 6,
  drop_na = FALSE,
  sort = FALSE
)

prioritise(match_col, priority, separator, no_match = NA, na_string = "NA")

nothing()

count_records()

select_grade(grade_col, keep_NA = FALSE, upper_case = TRUE)
```

## Arguments

- ties:

  (logical) If TRUE then all records matching the tied groups are
  returned. Otherwise the first record is returned.

- na.rm:

  (logical) If TRUE then NA is ignored

- separator:

  (character, NULL) if !NULL this string is used to collapse matches
  with the same priority

- na_string:

  (character) NA values are replaced with this string

- max_col:

  (character) the column name to search for the maximum value.

- use_abs:

  (logical) If TRUE then the sign of the values is ignored.

- keep_NA:

  (logical) If TRUE keeps records with NA values

- unique:

  (logical) If TRUE then exactly one record is returned even when
  multiple records tie, by taking the first matching record. This
  guarantees a single, deterministic and reproducible result per group
  instead of one row per tied record.

- min_col:

  (character) the column name to search for the minimum value.

- match_col:

  (character) the column with labels to prioritise

- search_col:

  (character) the name of a column to use as a reference for locating
  values in the matching column.

- match:

  (character) a value to search for in the matching column.

- digits:

  (numeric) the number of digits to use when converting numerical values
  to characters when determining if values are unique.

- drop_na:

  (logical) exclude NA from the list of unique entires

- sort:

  (logical) sort the values before collapsing.

- priority:

  (character) a list of labels in priority order

- no_match:

  (character, NULL) if !NULL then annotations not matching any of the
  priority labels are replaced with this value

- grade_col:

  (character) the name of a column containing grades

- upper_case:

  (logical) If TRUE then grades are compared to upper case letters to
  determine their ordering, otherwise lower case.

## Value

A function for use with
[`combine_records()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/combine_records.md)

## Functions

- `compute_mode()`: returns the most common value, excluding NA. If
  `ties == TRUE` then all tied values are returned, otherwise the first
  value in a sorted unique list is returned (equal to min if numeric).
  If `na.rm = FALSE` then NA are included when searching for the modal
  value and placed last if `ties = FALSE` (values are returned
  preferentially over NA).

- `compute_mean()`: calculates the mean value, excluding NA if
  `na.rm = TRUE`

- `compute_median()`: calculates the median value, excluding NA if
  `na.rm = TRUE`

- `fuse()`: collapses multiple matching records into a single string
  using the provided separator.

- `select_max()`: selects a record based on the index of the maximum
  value in a another column.

- `select_min()`: selects a record based on the index of the minimum in
  a second column.

- `select_match()`: returns all records based on the indices of
  identical matches in a second column and collapses them using the
  provided separator.

- `select_exact()`: returns records based on the index of identical
  value matching the `match` parameter within the current column, and
  collapses them using the provided separator if necessary.

- `fuse_unique()`: collapses a set of records to a set of unique values
  using the provided separator. `digits` can be provided for numeric
  columns to control the precision used when determining unique values.

- `prioritise()`: reduces a set of annotations by prioritising values
  according to the input. If there are multiple matches with the same
  priority then they are collapsed using a separator.

- `nothing()`: a pass-through function to allow some annotation table
  columns to remain unchanged.

- `count_records()`: adds a new column indicating the number of
  annotations that match the given grouping variable.

- `select_grade()`: returns records based on the index of the best grade
  in a second list. The best grade is defined as "A" for
  `upper_case = TRUE` or "a" for `upper_case = FALSE` and the worst
  grade is "Z" or "z". Any non-exact matches to a character in `LETTERS`
  or `letters` are replaced with NA.

## Examples

``` r

# Select matching records
M <- combine_records(
    group_by = "example",
    default_fcn = select_exact(
        match_col = "match_column",
        match = "find_me",
        separator = ", ",
        na_string = "NA"
    )
)

# Collapse unique values
M <- combine_records(
    group_by = "example",
    default_fcn = fuse_unique(
        digits = 6,
        separator = ", ",
        na_string = "NA",
        sort = FALSE
    )
)

# Prioritise by source
M <- combine_records(
    group_by = "InChiKey",
    default_fcn = prioritise(
        match_col = "source",
        priority = c("CD", "LS"),
        separator = "  || "
    )
)

# Do nothing to all columns
M <- combine_records(
    group_by = "InChiKey",
    default_fcn = nothing()
)

# Add a column with the number of records with a matching inchikey
M <- combine_records(
    group_by = "InChiKey",
    fcns = list(
        count = count_records()
    )
)

# Select annotation with highest (best) grade
M <- combine_records(
    group_by = "InChiKey",
    default_fcn = select_grade(
        grade_col = "grade",
        keep_NA = FALSE,
        upper_case = TRUE
    )
)
```
