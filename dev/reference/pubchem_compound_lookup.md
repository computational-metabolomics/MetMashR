# Compound ID lookup via PubChem

Uses the PubChem API to search for CID based on the input annotation
column.

## Usage

``` r
pubchem_compound_lookup(
  query_column,
  search_by,
  suffix = "_pubchem",
  output = "cids",
  records = "best",
  ...
)
```

## Arguments

- query_column:

  (character) The column name to use as the reference for searching the
  database e.g. "HMBD_ID".

- search_by:

  (character) The PubChem domain to search for matches to the
  annotation_column.

- suffix:

  (character) A suffix appended to all column names in the returned
  result. The default is `"_pubchem"`.

- output:

  (character) The value returned from the pubchem database. The default
  is `"cids"`.

- records:

  (character) Returned record(s). Allowed values are limited to the
  following:

  - `""`: Sometimes there are multiple matches to the PubChem, database
    especially when searhcing by name.

  - `"best"`: Return only the best matching record.

  - `"all"`: Return all matching records.

  The default is `"best"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `pubchem_compound_lookup` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The annotation_source after adding data returned by the API. |

## Inheritance

A `pubchem_compound_lookup` object inherits the following `struct`
classes:\
\
`[pubchem_compound_lookup]` -\> `[rest_api]` -\> `[model]` -\>
`[struct_class]`

## Examples

``` r
M <- pubchem_compound_lookup(
        search_by = "cid",
        output = "cids",
        records = "best",
        base_url = "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound",
        url_template = "<base_url>/<search_by>/<query_column>/<output>/JSON",
        query_column = character(0),
        cache = NULL,
        cache_mode = "update",
        status_codes = list(),
        delay = 0.5,
        suffix = "_rest_api")
```
