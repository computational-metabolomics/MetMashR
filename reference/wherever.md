# Filter helper function to select records

Returns a list of quosures for use with `filter_records` to allow the
use of dplyr-style expressions. See examples.

## Usage

``` r
wherever(...)
```

## Arguments

- ...:

  Expressions that return a logical value and are defined in terms of
  the columns in the annotation_source. If multiple conditions are
  included then they are combined with the `&` operator. Only records
  for which all conditions evaluate to `TRUE` are kept.

## Value

a list of quosures for use with `filter_records`

## See also

[`filter_records()`](https://computational-metabolomics.github.io/MetMashR/reference/filter_records.md)

## Examples

``` r
# some annotation data
AN <- annotation_source(data = iris)

# filter to setosa where Sepal length is less than 5
M <- filter_records(
    wherever(
        Species == "setosa",
        Sepal.Length < 5
    )
)
M <- model_apply(M, AN)
predicted(M) # 20 rows
#> A "annotation_source" object
#> ----------------------------
#> name:          An annotation source
#> description:   A base class defining an annotation source. This object is extended by MetmashR to define
#>                  other objects.
#> input params:  tag, data, source 
#> annotations:   20 rows x 5 columns
```
