# Cached database

A cached resource using BiocFileCache.

## Usage

``` r
BiocFileCache_database(
  source,
  bfc_path = NULL,
  resource_name,
  bfc_fun = cache_as_is,
  import_fun = read.csv,
  offline = FALSE,
  ...
)
```

## Arguments

- source:

  (ANY) The source of annotation data.

- bfc_path:

  (character, NULL) `BiocFileCache` is used to cache the database
  locally and prevent unnecessary downloads. If a path is provided then
  `BiocFileCache` will use this location. If NULL it will use the
  default location (see
  [`BiocFileCache::BiocFileCache()`](https://rdrr.io/pkg/BiocFileCache/man/BiocFileCache-class.html)
  for details). The default is `NULL`.

- resource_name:

  (character) The name given to this resource in the cache. (see
  [`BiocFileCache::BiocFileCache()`](https://rdrr.io/pkg/BiocFileCache/man/BiocFileCache-class.html)
  for details).

- bfc_fun:

  (function) A function to process the object before storing it in the
  cache, e.g. to store an unzipped file in the cache instead of the
  zipped version. This would prevent needing to unzip the resource each
  time it is retrieved from the cache, but would mean using more space
  on disk. The default function does nothing to the resource. See
  [`BiocFileCache::bfcdownload()`](https://rdrr.io/pkg/BiocFileCache/man/BiocFileCache-class.html)
  for details.

- import_fun:

  (function) A function to process the object after retrieving it from
  the cache e.g. it might need to be unzipped before importingas a
  data.frame. This function should take the path to the cached object as
  the first input and return a data.frame.

- offline:

  (logical) If `offline = FALSE` then checks to determine if the
  resource has expired will be skipped, and retrieved directly from the
  cache. The default is `FALSE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` BiocFileCache_database ` object. This object has no `output` slots.

## Details

This object makes use of functionality from the following packages:

- `BiocFileCache`

## Inheritance

A `BiocFileCache_database` object inherits the following `struct`
classes:\
\
`[BiocFileCache_database]` -\> `[annotation_database]` -\>
`[annotation_source]` -\> `[struct_class]`

## References

Shepherd L, Morgan M (2026). *BiocFileCache: Manage Files Across
Sessions*. R package version 3.2.0.

## See also

Other database:
[`lipidmaps_database()`](https://computational-metabolomics.github.io/MetMashR/reference/lipidmaps_database.md),
[`sqlite_database()`](https://computational-metabolomics.github.io/MetMashR/reference/sqlite_database.md)

## Examples

``` r
M <- BiocFileCache_database(
        bfc_path = NULL,
        resource_name = "bfc",
        bfc_fun = function(){},
        import_fun = function(){},
        offline = FALSE,
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
