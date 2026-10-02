# GitHub file

Uses the GitHub REST API to retrieve a file from a specifiedGitHub
repository.

## Usage

``` r
github_file(
  username,
  repository_name,
  file_path,
  bfc_path = NULL,
  resource_name = paste(username, repository_name, file_path, sep = "_"),
  ...
)
```

## Arguments

- username:

  (character) The GitHub username to retireve the file from.

- repository_name:

  (character) The name of a repository for the specified GitHub
  usernamethat contains the file to download.

- file_path:

  (character) The path to the file to download within the specified
  GitHub repository.

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
  `paste(username, repository_name, file_path, sep = "_")`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` github_file ` object. This object has no `output` slots.

## Details

This object makes use of functionality from the following packages:

- `BiocFileCache`

- `httr`

## Inheritance

A `github_file` object inherits the following `struct` classes:\
\
`[github_file]` -\> `[BiocFileCache_database]` -\>
`[annotation_database]` -\> `[annotation_source]` -\> `[struct_class]`

## References

Shepherd L, Morgan M (2026). *BiocFileCache: Manage Files Across
Sessions*. R package version 3.2.0.

Wickham H (2026). *httr: Tools for Working with URLs and HTTP*.
doi:10.32614/CRAN.package.httr
<https://doi.org/10.32614/CRAN.package.httr>. R package version 1.4.9,
<https://CRAN.R-project.org/package=httr>.

## Examples

``` r
M <- github_file(
        username = character(0),
        repository_name = character(0),
        file_path = character(0),
        bfc_path = NULL,
        resource_name = "bfc",
        bfc_fun = function(){},
        import_fun = function(){},
        offline = FALSE,
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
