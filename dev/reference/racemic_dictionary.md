# Racemic dictionary

This dictionary removes racemic properties from molecule names. It is
intended for use with the
[`normalise_strings()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/normalise_strings.md)
object.

## Usage

``` r
racemic_dictionary
```

## Value

A dictionary for use with
[`normalise_strings()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/normalise_strings.md)

## Examples

``` r
M <- normalise_strings(
    search_column = "example",
    output_column = "result",
    dictionary = racemic_dictionary
)
```
