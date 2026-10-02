# Tripeptide dictionary

A dictionary for converting tripeptides encoded using single letter
IUPAC codes to use three letter codes for amino acids separated by
hyphens. e.g. INK becomes Ile-Asn-Lys

## Usage

``` r
tripeptide_dictionary
```

## Value

A dictionary for use with
[`normalise_strings()`](https://computational-metabolomics.github.io/MetMashR/reference/normalise_strings.md)

## Examples

``` r
M <- normalise_strings(
    search_column = "example",
    output_column = "result",
    dictionary = tripeptide_dictionary
)
```
