# mwb_refmet_database

Imports the Metabolomics Workbench refmet database.

## Usage

``` r
mwb_refmet_database(bfc = NULL, ...)
```

## Arguments

- bfc:

  (character) `BiocFileCache` is used to cache database locally and
  prevent unnecessary downloads. If a path is provided then
  `BiocFileCache` will use this location. If NULL it will use the
  default location (see
  [BiocFileCache::BiocFileCache](https://rdrr.io/pkg/BiocFileCache/man/BiocFileCache-class.html)
  for details). The default is `NULL`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` mwb_refmet_database ` object. This object has no `output` slots.

## Details

This object makes use of functionality from the following packages:

- `BiocFileCache`

- `httr`

- `plyr`

## Inheritance

A `mwb_refmet_database` object inherits the following `struct` classes:\
\
`[mwb_refmet_database]` -\> `[annotation_database]` -\>
`[annotation_source]` -\> `[struct_class]`

## References

Shepherd L, Morgan M (2026). *BiocFileCache: Manage Files Across
Sessions*. R package version 3.2.0.

Wickham H (2026). *httr: Tools for Working with URLs and HTTP*.
doi:10.32614/CRAN.package.httr
<https://doi.org/10.32614/CRAN.package.httr>. R package version 1.4.9,
<https://CRAN.R-project.org/package=httr>.

Wickham H (2011). "The Split-Apply-Combine Strategy for Data Analysis."
*Journal of Statistical Software*, *40*(1), 1-29.
<https://www.jstatsoft.org/v40/i01/>.

## Examples

``` r
M <- mwb_refmet_database(
        bfc = character(0),
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
