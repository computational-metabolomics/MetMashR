# Greek dictionary

A dictionary for converting Greek characters to Romanised names. It is
intended for use with the
[`normalise_strings()`](https://computational-metabolomics.github.io/MetMashR/reference/normalise_strings.md)
object.

## Usage

``` r
greek_dictionary
```

## Value

A dictionary for use with
[`normalise_strings()`](https://computational-metabolomics.github.io/MetMashR/reference/normalise_strings.md)

## Examples

``` r
M <- normalise_strings(
    search_column = "example",
    output_column = "result",
    dictionary = greek_dictionary
)
```
