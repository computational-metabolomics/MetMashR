# Zenodo file

Retrieves a file from a Zenodo record and caches it locally using
`BiocFileCache`.

## Usage

``` r
zenodo_file(
  record_id,
  file_name,
  bfc_path = NULL,
  resource_name = paste("zenodo", record_id, file_name, sep = "_"),
  ...
)
```

## Arguments

- record_id:

  (character, numeric) The ID of the Zenodo record containing the file,
  e.g. 8226097 for doi:10.5281/zenodo.8226097. Each version of a Zenodo
  record has its own ID.

- file_name:

  (character) The name of the file to download from the Zenodo record.

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
  for details). The default is
  `paste("zenodo", record_id, file_name, sep = "_")`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` zenodo_file ` object. This object has no `output` slots.

## Details

This object makes use of functionality from the following packages:

- `BiocFileCache`

## Inheritance

A `zenodo_file` object inherits the following `struct` classes:\
\
`[zenodo_file]` -\> `[BiocFileCache_database]` -\>
`[annotation_database]` -\> `[annotation_source]` -\> `[struct_class]`

## References

Shepherd L, Morgan M (2026). *BiocFileCache: Manage Files Across
Sessions*. R package version 3.2.0.

## Examples

``` r
M <- zenodo_file(
        record_id = character(0),
        file_name = character(0),
        bfc_path = NULL,
        resource_name = "bfc",
        bfc_fun = function(){},
        import_fun = function(){},
        offline = FALSE,
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
