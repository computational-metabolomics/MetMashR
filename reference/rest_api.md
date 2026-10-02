# rest_api

A base class providing common methods for making REST API calls.

## Usage

``` r
rest_api(
  base_url,
  url_template,
  suffix,
  status_codes,
  delay,
  cache = NULL,
  cache_mode = "update",
  query_column,
  ...
)
```

## Arguments

- base_url:

  (character) The base URL of the API.

- url_template:

  (character) A template describing how the URL should be constructed
  from the base URL and input parameters. e.g.
  \<base_url\>//\<input_item\>/\<search_term\>/json.The url will be
  constructed by replacing the values enclosed in \<\> with the value
  from corresponding input parameter of the rest_api object.

- suffix:

  (character) A suffix appended to all column names in the returned
  result.

- status_codes:

  (list) Named list of status codes and function indicating how to
  respond. Should minimally contain a function to parse a successful
  response for status code 200. Any codes not provided will be passed to
  httr::stop_for_status().

- delay:

  (numeric, integer) Delay in seconds between API calls.

- cache:

  (annotation_database, NULL) A struct cache object that contains parsed
  responses to previous api queries. If not using a cache then set to
  NULL. The default is `NULL`.

- cache_mode:

  (character) Cache mode. Allowed values are limited to the following:

  - `"update"`: The normal mode: values already in `cache` are used
    as-is, anything missing is queried live and the result added to the
    cache.

  - `"offline"`: Never query the live API - only values already present
    in `cache` are returned, and everything else is left as NA. Useful
    for continuing to work with a partially-populated cache while the
    API is unreachable or down, without waiting on or erroring against
    the live service. A warning lists how many query values are not
    covered by the cache when this happens (and, if the cache is
    entirely empty or unset, that every value will be returned as NA).

  - `"rebuild"`: Ignore any existing cached value and query the live API
    for every value, overwriting the corresponding entry in `cache`.
    Useful when cached results are known to be stale.

  The default is `"update"`.

- query_column:

  (character) The name of a column in the annotation table containing
  values to search in the api call.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `rest_api` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The annotation_source after adding data returned by the API. |

## Inheritance

A `rest_api` object inherits the following `struct` classes:\
\
`[rest_api]` -\> `[model]` -\> `[struct_class]`

## See also

Other REST API's:
[`classyfire_batch_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/classyfire_batch_lookup.md),
[`classyfire_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/classyfire_lookup.md),
[`cts_lite_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/cts_lite_lookup.md),
[`kegg_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/kegg_lookup.md),
[`lipidmaps_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/lipidmaps_lookup.md),
[`mwb_compound_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_compound_lookup.md),
[`mwb_refmet_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_refmet_lookup.md),
[`pubchem_id_exchange()`](https://computational-metabolomics.github.io/MetMashR/reference/pubchem_id_exchange.md)

## Examples

``` r
M <- rest_api(
        base_url = "V1",
        url_template = character(0),
        query_column = character(0),
        cache = NULL,
        cache_mode = "update",
        status_codes = list(),
        delay = 0.5,
        suffix = "_rest_api")
```
