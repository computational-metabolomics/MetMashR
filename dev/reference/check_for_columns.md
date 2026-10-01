# Check for columns in an `annotation_source`

This method checks for the presence of columns by name in an
[`annotation_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/annotation_source.md).
It returns TRUE if all are present, or a vector of messages indicating
which columns are missing from the data.frame. It is used by MetMashR to
ensure validity of certain objects.

## Usage

``` r
check_for_columns(obj, ..., msg = FALSE)

# S4 method for class 'annotation_source'
check_for_columns(obj, ..., msg = FALSE)
```

## Arguments

- obj:

  an
  [`annotation_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/annotation_source.md)
  object

- ...:

  the column names to check for

- msg:

  TRUE/FALSE indicates whether to return a message if some columns are
  missing. If `msg = FALSE` then the function returns FALSE if all
  columns are not present.

## Value

logical if all columns are present, or a vector of messages if
requested.

## Examples

``` r
# test if column present
AT <- annotation_source(data = data.frame(id = character(0)))
check_for_columns(AT, "id") # TRUE
#> [1] TRUE
check_for_columns(AT, "cake") # FALSE
#> [1] FALSE

# return a message if missing
check_for_columns(AT, "cake", msg = TRUE)
#> [1] "The following columns are missing from the data.frame: \"cake\""
```
