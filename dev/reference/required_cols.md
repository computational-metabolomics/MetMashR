# Required columns in an annotation source

Some `annotation_sources`, such as LCMS tables (`lcms_table`), require
that certain columns are present in the data.frame. These are defined by
slots in the source definition. The name of slots containing the
required column names for a source can be retrieved using the
`required_cols` function, which will collect and return the names of
slots containing required column names for the object and all of its
parent objects.

## Usage

``` r
required_cols(obj, ...)

# S4 method for class 'annotation_source'
required_cols(obj)
```

## Arguments

- obj:

  an `annotation_source` object

- ...:

  additional source specific inputs

## Value

a character vector of slot names

## Examples

``` r
# prepare object
M <- lcms_table(id_column = "id", mz_column = "mz", rt_column = "rt")

#' # get values for required slots
r <- required_cols(M)

# get slot names for required columns
names(r)
#> NULL
```
