# ClassyFire batch lookup

Uses ClassyFire's batch submission API to obtain chemical ontology
information (kingdom/superclass/class/...) for many SMILES in one or a
few requests, rather than one request per compound (see
[`classyfire_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/classyfire_lookup.md)).

## Usage

``` r
classyfire_batch_lookup(
  query_column,
  output_items = c("kingdom", "superclass", "class"),
  output_fields = "name",
  suffix = "_cfb",
  delay = 3,
  max_poll_rounds = 30,
  n_batches = NULL,
  cache = NULL,
  cache_mode = "update",
  verbose = FALSE,
  ...
)

# S4 method for class 'classyfire_batch_lookup,annotation_source'
model_train(M, D)

# S4 method for class 'classyfire_batch_lookup,annotation_source'
model_apply(M, D)

# S4 method for class 'classyfire_batch_lookup,annotation_source'
model_predict(M, D)
```

## Arguments

- query_column:

  (character) The name of a column in the annotation table containing
  SMILES strings to submit to ClassyFire.

- output_items:

  (character) The names of the items to return: "kingdom", "superclass",
  "class", "subclass" and/or "direct_parent" - the taxonomy items
  ClassyFire returns as a nested name/description/chemont_id/url object
  per compound. Keyword ".all" returns all of them. Other ClassyFire
  fields (substituents, ancestors, ...) are array-valued per compound
  rather than a single nested object and are not supported by this
  object. The default is `c("kingdom", "superclass", "class")`.

- output_fields:

  (character) The fields to return for each output_item, where
  applicable. Can include "name", "description", "chemont_id" and "url".
  Keyword ".all" returns all fields. The default is `"name"`.

- suffix:

  (character) A suffix appended to all column names in the returned
  result. The default is `"_cfb"`.

- delay:

  (numeric, integer) Delay in seconds between status polling requests.
  The default is `3`.\

- max_poll_rounds:

  (numeric, integer) Maximum number of polling rounds before giving up.
  The default is `30`.\

- n_batches:

  (numeric, integer, NULL) Optional number of submissions to split the
  unique query values across. If NULL, all query values are submitted in
  a single request. The default is `NULL`.

- cache:

  (annotation_database, NULL) A struct cache object (e.g.
  [`rds_cache()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/rds_cache.md))
  that stores parsed responses to previous ClassyFire queries, keyed by
  query value. Values already present in the cache are not resubmitted.
  Besides speeding up re-runs, this keeps a local, persistent record of
  what ClassyFire returned - useful given the service can go down or
  change without notice, as happened to the Chemical Translation Service
  this package once depended on (see
  [`mwb_refmet_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/mwb_refmet_lookup.md)).
  If not using a cache then set to NULL. The default is `NULL`.

- cache_mode:

  (character) Cache mode. Allowed values are limited to the following:

  - `"update"`: The normal mode: values already in `cache` are used
    as-is, anything missing is submitted to ClassyFire and the result
    added to the cache.

  - `"offline"`: Never submit a batch to ClassyFire - only values
    already present in `cache` are returned, and everything else is left
    as NA. Useful for continuing to work with a partially-populated
    cache while ClassyFire is unreachable or down, without waiting on or
    erroring against the live service. A warning lists how many query
    values are not covered by the cache when this happens (and, if the
    cache is entirely empty or unset, that every value will be returned
    as NA).

  - `"rebuild"`: Ignore any existing cached value and submit every value
    to ClassyFire, overwriting the corresponding entry in `cache`.
    Useful when cached results are known to be stale.

  The default is `"update"`.

- verbose:

  (logical) Whether to print debug information during execution. The
  default is `FALSE`.\

- ...:

  Additional slots and values passed to `struct_class`.

- M:

  A `classyfire_batch_lookup` object.

- D:

  An
  [`annotation_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/annotation_source.md)
  object.

## Value

A `classyfire_batch_lookup` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The annotation_source after adding data returned by ClassyFire. |
| `query_batches` | (list) The query values submitted in each batch during training (excludes any already present in `cache`). |
| `query_values` | (character) Every unique, non-missing value of `query_column` seen during training (cached or not) - used to reconstruct the full result set in `model_predict`. |
| `request_ids` | (list) ClassyFire query ids for each submitted batch. |
| `trained` | (logical) Whether the model has already submitted its ClassyFire jobs. |

## Details

ClassyFire's per-compound endpoint (see
[`classyfire_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/classyfire_lookup.md))
only accepts one InChIKey per request and is aggressively rate-limited,
making it impractical for more than a few dozen compounds. ClassyFire
also provides a batch submission API
([wishartlab/classyfire_api](https://bitbucket.org/wishartlab/classyfire_api/src/master/))

- `POST .../queries.json` with many structures at once, then poll
  `GET .../queries/<id>.json` until finished - which this object uses
  instead: one submission (or a handful, via `n_batches`) covers the
  whole input. This batch endpoint takes a **SMILES or InChI** string
  (not a bare InChIKey) per compound; each is submitted as an
  `<value><TAB><value>` pair so ClassyFire echoes the value back as its
  `identifier`, letting results be matched back to the original rows by
  exact equality rather than relying on ClassyFire's undocumented
  internal ordering.

## Inheritance

A `classyfire_batch_lookup` object inherits the following `struct`
classes:\
\
`[classyfire_batch_lookup]` -\> `[model]` -\> `[struct_class]`

## See also

Other REST API's:
[`classyfire_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/classyfire_lookup.md),
[`cts_lite_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/cts_lite_lookup.md),
[`kegg_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/kegg_lookup.md),
[`lipidmaps_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/lipidmaps_lookup.md),
[`mwb_compound_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/mwb_compound_lookup.md),
[`mwb_refmet_lookup()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/mwb_refmet_lookup.md),
[`pubchem_id_exchange()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/pubchem_id_exchange.md),
[`rest_api()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/rest_api.md)

## Examples

``` r
M <- classyfire_batch_lookup(
        query_column = character(0),
        output_items = c("kingdom", "superclass", "class"),
        output_fields = "name",
        suffix = "_cfb",
        delay = 3,
        max_poll_rounds = 30,
        n_batches = NULL,
        cache = NULL,
        cache_mode = "update",
        verbose = FALSE)
```
