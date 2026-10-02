# NCBI E-utils query

Submit a query to one of the NCBI E-utils databases. See
https://www.ncbi.nlm.nih.gov/books/NBK25501/ for details.

## Usage

``` r
eutils_lookup(query_column, database, term, result_fields = "idlist", ...)
```

## Arguments

- query_column:

  (character) The column name to use as the reference for searching the
  database e.g. "HMBD_ID".

- database:

  (character) The name of the E-utils database to search. See
  https://www.ncbi.nlm.nih.gov/books/NBK25501/ for details.

- term:

  (character) A correctly formated search term to use with E-utils. See
  https://www.ncbi.nlm.nih.gov/books/NBK25501/ for details. When used
  with the provided url template will automatically include the value
  from the `query_column` at the beginning of the term.

- result_fields:

  (character) The name of the search result field to return. For E-utils
  this is often "idlist". The default is `"idlist"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `eutils_lookup` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The annotation_source after adding data returned by the API. |

## Inheritance

A `eutils_lookup` object inherits the following `struct` classes:\
\
`[eutils_lookup]` -\> `[rest_api]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- eutils_lookup(
        database = "gene",
        term = "[pdat]",
        result_fields = "idlist",
        base_url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils",
        url_template = paste0(
            "<base_url>/esearch.fcgi?db=<database>&",
            "term=<query_column><term>&retmode=json"
        ),
        query_column = character(0),
        cache = NULL,
        cache_mode = "update",
        status_codes = list(),
        delay = 0.5,
        suffix = "_rest_api")
```
