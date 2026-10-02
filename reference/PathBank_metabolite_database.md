# PathBank_metabolite_database

Imports the PathBank database (https://pathbank.org/) of metabolites
linked to pathways.

## Usage

``` r
PathBank_metabolite_database(
  version = "primary",
  bfc_path = NULL,
  resource_name = "MetMashR_PathBank",
  ...
)
```

## Arguments

- version:

  (character) PathBank version. Allowed values are limited to the
  following:

  - `""`: The version of the PatchBank database to import. To prevent
    unecessary downloads `BiocFileCache` is used to store a local copy.

  - `"complete"`: The complete PathBank metabolite database.

  - `"primary"`: The PathBank metabolite database for primary pathways
    only.

  The default is `"primary"`.

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
  for details). The default is `"MetMashR_PathBank"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` PathBank_metabolite_database ` object. This object has no `output`
slots.

## Details

This object makes use of functionality from the following packages:

- `BiocFileCache`

- `httr`

## Inheritance

A `PathBank_metabolite_database` object inherits the following `struct`
classes:\
\
`[PathBank_metabolite_database]` -\> `[BiocFileCache_database]` -\>
`[annotation_database]` -\> `[annotation_source]` -\> `[struct_class]`

## References

Shepherd L, Morgan M (2026). *BiocFileCache: Manage Files Across
Sessions*. R package version 3.2.0.

Wickham H (2026). *httr: Tools for Working with URLs and HTTP*.
doi:10.32614/CRAN.package.httr
<https://doi.org/10.32614/CRAN.package.httr>. R package version 1.4.9,
<https://CRAN.R-project.org/package=httr>.

Wishart, S D, Li, Carin, Marcu, Ana, Badran, Hasan, Pon, Allison,
Budinski, Zachary, Patron, Jonas, Lipton, Debra, Cao, Xuan, Oler,
Eponine, Li, Krissa, Paccoud, Maïlys, Hong, Chelsea, Guo, C A, Chan,
Christopher, Wei, William, Ramirez-Gaona, Miguel (2019). "PathBank: a
comprehensive pathway database for model organisms." *Nucleic Acids
Research*, *48*, D470-D478. doi:10.1093/nar/gkz861
<https://doi.org/10.1093/nar/gkz861>.

## Examples

``` r
M <- PathBank_metabolite_database(
        version = "primary",
        bfc_path = NULL,
        resource_name = "bfc",
        bfc_fun = function(){},
        import_fun = function(){},
        offline = FALSE,
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
