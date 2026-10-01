# GO.db

Retrieve a table from the Gene Ontology using the `GO.db` package.

## Usage

``` r
GO_database(source = "GO.db", table = "GOBPOFFSPRING", ...)
```

## Arguments

- source:

  (character) The name of an AnnotationDb package to import the
  specified table from. Note the package should already be installed.
  The default is `"GO.db"`.

- table:

  (character) The name of a table to import from the GO.db package.
  Allowed tables include:
  GOBPANCESTOR,GOBPPARENTS,GOBPCHILDREN,GOBPOFFSPRING (and their CC or
  MF equivalents), GOTERM, GOSYNONYM, GOOBSOLETE. The default is
  `"GOBPOFFSPRING"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` GO_database ` object. This object has no `output` slots.

## Details

This object makes use of functionality from the following packages:

- `GO.db`

## Inheritance

A `GO_database` object inherits the following `struct` classes:\
\
`[GO_database]` -\> `[AnnotationDb_database]` -\>
`[annotation_database]` -\> `[annotation_source]` -\> `[struct_class]`

## References

Carlson M (2026). *GO.db: A set of annotation maps describing the entire
Gene Ontology*. R package version 3.23.1.

Pagès H, Carlson M, Falcon S, Li N (2026). *AnnotationDbi: Manipulation
of SQLite-based annotations in Bioconductor*.
doi:10.18129/B9.bioc.AnnotationDbi
<https://doi.org/10.18129/B9.bioc.AnnotationDbi>. R package version
1.74.0, <https://bioconductor.org/packages/AnnotationDbi>.

## See also

GO.db

Other annotation databases:
[`AnnotationDb_database()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/AnnotationDb_database.md),
[`annotation_database()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/annotation_database.md),
[`annotation_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/annotation_source.md),
[`excel_database()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/excel_database.md),
[`rdata_database()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/rdata_database.md),
[`rds_cache()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/rds_cache.md),
[`rds_database()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/rds_database.md)

## Examples

``` r
M <- GO_database(
        table = "GOBPCHILDREN",
        tag = character(0),
        data = data.frame(),
        source = character(0))
```
