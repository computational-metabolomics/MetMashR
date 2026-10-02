# Compound ID lookup via pubchem

Requests HMBD records based on HMDB identifiers.

## Usage

``` r
hmdb_lookup(query_column, suffix = "_hmdb", output = "inchikey", ...)
```

## Arguments

- query_column:

  (character) The name of a column in the annotation table containing
  values to search in the api call.

- suffix:

  (character) A suffix appended to all column names in the returned
  result. The default is `"_hmdb"`.

- output:

  (character) The value returned from the HMDB xml. The default is
  `"inchikey"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `hmdb_lookup` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The annotation_source after adding data returned by the API. |

## Details

This object makes use of functionality from the following packages:

- `XML`

## Inheritance

A `hmdb_lookup` object inherits the following `struct` classes:\
\
`[hmdb_lookup]` -\> `[rest_api]` -\> `[model]` -\> `[struct_class]`

## References

Temple Lang D (2026). *XML: Tools for Parsing and Generating XML Within
R and S-Plus*. doi:10.32614/CRAN.package.XML
<https://doi.org/10.32614/CRAN.package.XML>. R package version
3.99-0.25, <https://CRAN.R-project.org/package=XML>.

## Examples

``` r
M <- hmdb_lookup(
        output = "inchikey",
        base_url = "http://www.hmdb.ca/metabolites",
        url_template = "<base_url>/<query_column>.xml",
        query_column = character(0),
        cache = NULL,
        cache_mode = "update",
        status_codes = list(),
        delay = 0.5,
        suffix = "_rest_api")
```
