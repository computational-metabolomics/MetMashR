# ID lookup by database

Search a database (data.frame) for annotation matches based on values in
a specified column.

## Usage

``` r
database_lookup(
  query_column,
  database_column,
  database,
  include = NULL,
  suffix = NULL,
  not_found = NA,
  ...
)
```

## Arguments

- query_column:

  (character) The annotation table column name to use as the reference
  for searching the database e.g. "HMBD_ID".

- database_column:

  (character) The database column to search for matches to the values in
  annoation_column.

- database:

  (data.frame, annotation_database) A database to be searched. Can be a
  `data.frame` or a `annotation_database` object.

- include:

  (character, NULL) The name of the database columns to be added to the
  annotations. If NULL, all columns are retained. The default is `NULL`.

- suffix:

  (character, NULL) A string appended to the column names from the
  database. Used to distinguish columns from different databases with
  identical column names.If suffix = NULL then the column names are not
  changed. The default is `NULL`.

- not_found:

  (character, numeric, logical, NULL) The returned value when there are
  no matches. The default is `NA`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `database_lookup` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The input annotation_source is updated with matching columns from the database. |

## Details

This object makes use of functionality from the following packages:

- `dplyr`

## Inheritance

A `database_lookup` object inherits the following `struct` classes:\
\
`[database_lookup]` -\> `[model]` -\> `[struct_class]`

## References

Wickham H, François R, Henry L, Müller K, Vaughan D (2026). *dplyr: A
Grammar of Data Manipulation*. doi:10.32614/CRAN.package.dplyr
<https://doi.org/10.32614/CRAN.package.dplyr>. R package version 1.2.1,
<https://CRAN.R-project.org/package=dplyr>.

## Examples

``` r
M <- database_lookup(
        query_column = "V1",
        database_column = "",
        database = data.frame(),
        include = NULL,
        suffix = NULL,
        not_found = NULL)
```
