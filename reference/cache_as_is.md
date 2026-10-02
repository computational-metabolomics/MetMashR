# Cache file with no changes using BiocFileCache

This helper function is for use with `BiocFileCache` objects. Using it
will copy the file directly to the cache without making any changes.

## Usage

``` r
cache_as_is(from, to)
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
    bfc_fun = cache_as_is
)
```
