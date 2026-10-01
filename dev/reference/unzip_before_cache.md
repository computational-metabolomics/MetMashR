# Unzip file before caching with BiocFileCache_database

This helper function is for use with
[`BiocFileCache_database()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/BiocFileCache_database.md)
objects. Using it as the `bfc_fun` input for this object will unzip a
downloaded resource into a temporary folder before storing it in the
cache.

## Usage

``` r
unzip_before_cache(from, to)
```

## Arguments

- from:

  incoming path

- to:

  the outgoing path

## Value

TRUE if successful

## Examples

``` r
M <- BiocFileCache_database(
    source = tempfile(),
    resource_name = "example",
    bfc_fun = unzip_before_cache
)
```
