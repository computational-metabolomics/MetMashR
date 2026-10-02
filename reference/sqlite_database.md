# SQLite database

A data.frame stored in an SQLite database.

## Usage

``` r
sqlite_database(source, table = "annotation_database", ...)
```

## Arguments

- source:

  (ANY) The source of annotation data.

- table:

  (character) The name of a table in the SQLite database. The default is
  `"annotation_database"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` sqlite_database ` object. This object has no `output` slots.

## Details

This object makes use of functionality from the following packages:

- `RSQLite`

## Inheritance

A `sqlite_database` object inherits the following `struct` classes:\
\
`[sqlite_database]` -\> `[annotation_database]` -\>
`[annotation_source]` -\> `[struct_class]`

## References

Müller K, Wickham H, James DA, Falcon S (2026). *RSQLite: SQLite
Interface for R*. doi:10.32614/CRAN.package.RSQLite
<https://doi.org/10.32614/CRAN.package.RSQLite>. R package version
3.53.3, <https://CRAN.R-project.org/package=RSQLite>.

## See also

Other database:
[`BiocFileCache_database()`](https://computational-metabolomics.github.io/MetMashR/reference/BiocFileCache_database.md),
[`lipidmaps_database()`](https://computational-metabolomics.github.io/MetMashR/reference/lipidmaps_database.md)

## Examples

``` r
M <- sqlite_database(
        table = character(0),
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
