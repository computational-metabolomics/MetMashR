# MTox700plus_database

Imports the MTox700+ database, which is made available under the ODC
Attribution License. MTox700+ is a list of toxicologically relevant
metabolites derived from publications, public databases and relevant
toxicological assays.

## Usage

``` r
MTox700plus_database(
  version = "latest",
  bfc_path = NULL,
  resource_name = "MetMashR_MTox700plus",
  ...
)
```

## Arguments

- version:

  (character) The version number of the MTox700+ database to import.
  Available versions are listed
  [here](https://github.com/michabohealthscience/MTox700plus/releases).
  `version` should match the tag of the release e.g. `"v1.0"`. For
  convenience `version = "latest"` will always retrieve the most recent
  release. To prevent unecessary downloads `BiocFileCache` is used to
  store a local copy. The default is `"latest"`.

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
  for details). The default is `"MetMashR_MTox700plus"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` MTox700plus_database ` object. This object has no `output` slots.

## Details

This object makes use of functionality from the following packages:

- `BiocFileCache`

- `httr`

## Inheritance

A `MTox700plus_database` object inherits the following `struct`
classes:\
\
`[MTox700plus_database]` -\> `[BiocFileCache_database]` -\>
`[annotation_database]` -\> `[annotation_source]` -\> `[struct_class]`

## References

Shepherd L, Morgan M (2026). *BiocFileCache: Manage Files Across
Sessions*. R package version 3.2.0.

Wickham H (2026). *httr: Tools for Working with URLs and HTTP*.
doi:10.32614/CRAN.package.httr
<https://doi.org/10.32614/CRAN.package.httr>. R package version 1.4.9,
<https://CRAN.R-project.org/package=httr>.

Sostare E, Lawson TN, Saunders LR, Colbourne JK, Weber RJM, Sobanski T,
Viant MR (2022). "Knowledge-Driven Approaches to Create the MTox700+
Metabolite Panel for Predicting Toxicity." *Toxicological Sciences*,
*186*, 208-220. doi:10.1093/toxsci/kfac007
<https://doi.org/10.1093/toxsci/kfac007>.

## Examples

``` r
M <- MTox700plus_database(
        version = "v1.0",
        bfc_path = NULL,
        resource_name = "bfc",
        bfc_fun = function(){},
        import_fun = function(){},
        offline = FALSE,
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
