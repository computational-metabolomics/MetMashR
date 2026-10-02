# Join sources vertically

A function to join sources vertically. A vertical join involves matching
common columns across source data.frames and padding missing columns to
create a single new data.frame with data and records from multiple
sources.

## Usage

``` r
vertical_join(x, y, ...)

# S4 method for class 'annotation_source,annotation_source'
vertical_join(
  x,
  y,
  matching_columns = NULL,
  keep_cols = NULL,
  source_col = "annotation_source",
  exclude_cols = NULL,
  as = annotation_source()
)

# S4 method for class 'list,missing'
vertical_join(
  x,
  y,
  matching_columns = NULL,
  keep_cols = NULL,
  source_col = "annotation_source",
  exclude_cols = NULL,
  as = annotation_source()
)
```

## Arguments

- x:

  an `annotation_source` object

- y:

  an second `annotation_source` object to join with the first

- ...:

  additional inputs (not currently used)

- matching_columns:

  (list) a named list of column names that all contain the same
  information. All columns named in the same list element will be merged
  into a single column with the same name as the list element.

- keep_cols:

  (character) a list of column names to keep in the final joined table.
  All other columns will be dropped.

- source_col:

  (character) the name of a new column that will contain the tags of the
  original source object for each row in the joined table.

- exclude_cols:

  (character) the names of columns to exclude from the joined table.

- as:

  (character) the type of object the joined table should be returned as
  e.g. "lcms_table".

## Value

an `annotation_source` object

## Examples

``` r

M <- annotation_source(data = data.frame(id = 1, value = "A"))
N <- annotation_source(data = data.frame(id = 2, value = "B"))
O <- vertical_join(M, N, keep_cols = ".all")
```
