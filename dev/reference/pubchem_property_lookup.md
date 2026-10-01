# Compound property lookup via pubchem

Uses the PubChem API to search for CID based onthe input annotation
column and returns property information.

## Usage

``` r
pubchem_property_lookup(
  query_column,
  search_by,
  suffix = "_pubchem",
  property = "InChIKey",
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

- property:

  (character) A comma separated list of properties to return from the
  pubchem database. (see
  https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Compound-Property-Tables
  for details). Keyword ".all" will return all properties. The default
  is `"InChIKey"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `pubchem_property_lookup` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The annotation_source after adding data returned by the API. |

## Inheritance

A `pubchem_property_lookup` object inherits the following `struct`
classes:\
\
`[pubchem_property_lookup]` -\> `[pubchem_compound_lookup]` -\>
`[rest_api]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- pubchem_property_lookup(
        search_by = "cid",
        property = "InChIKey",
        output = "cids",
        records = "best",
        base_url = "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound",
        url_template = "<base_url>/<search_by>/<query_column>/property/<property>/JSON",
        query_column = character(0),
        cache = NULL,
        cache_mode = "update",
        status_codes = list(),
        delay = 0.5,
        suffix = "_rest_api")
```
