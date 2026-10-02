# PubChem ID Exchange via PUG

Uses the PubChem PUG API to perform ID exchange operations. Submits a
list of identifiers and retrieves corresponding identifiers with the
same chemical structure. Prediction data must contain the column
specified by query_column. The prediction input need not be the same
data used for training, but it must contain compatible query identifiers
for joining results.

## Usage

``` r
pubchem_id_exchange(
  query_column,
  output_type = "inchikey",
  suffix = "_pubchem_id_exchange",
  delay = 2,
  max_attempts = 30,
  max_poll_rounds = 10,
  input_type = "synonyms",
  input_source_name = NULL,
  output_source_name = NULL,
  n_batches = NULL,
  cache = NULL,
  cache_mode = "update",
  verbose = FALSE,
  ...
)
```

## Arguments

- query_column:

  (character) The name of a column in the annotation table containing
  values to search in the PUG API call.

- output_type:

  (character) The type of identifiers to return from the PUG API. The
  default is `"inchikey"`.

- suffix:

  (character) A suffix appended to all column names in the returned
  result. The default is `"_pubchem_id_exchange"`.

- delay:

  (numeric, integer) Delay in seconds between status polling requests.
  The default is `2`.\

- max_attempts:

  (numeric, integer) Maximum number of status polling attempts before
  giving up. The default is `30`.\

- max_poll_rounds:

  (numeric, integer) Maximum number of whole-batch polling rounds before
  giving up when some batches are still unavailable. The default is
  `10`.\

- input_type:

  (character) The type of identifiers being provided as input.
  Determines which XML structure to use for the query. The default is
  `"synonyms"`.

- input_source_name:

  (character, NULL) The name of the external registry source when using
  Registry IDs as input (input_type = 'source-ids'). Required when
  input_type is 'source-ids'. The default is `NULL`.

- output_source_name:

  (character, NULL) The name of the external registry source when
  requesting Registry IDs as output (output_type = 'regid'). Required
  when output_type is 'regid'. The default is `NULL`.

- n_batches:

  (numeric, integer, NULL) Optional number of POST batches to split the
  unique query values across. If NULL, all query values are submitted in
  a single request. The default is `NULL`.

- cache:

  (annotation_database, NULL) A struct cache object (e.g.
  [`rds_cache()`](https://computational-metabolomics.github.io/MetMashR/reference/rds_cache.md))
  that stores parsed responses to previous PubChem ID exchange queries,
  keyed by query value. Values already present in the cache are not
  resubmitted. Besides speeding up re-runs, this keeps a local,
  persistent record of what PubChem returned - useful given a
  translation service can go down or change without notice, as happened
  to the Chemical Translation Service this package once depended on (see
  [`mwb_refmet_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_refmet_lookup.md)).
  If not using a cache then set to NULL. The default is `NULL`.

- cache_mode:

  (character) Cache mode. Allowed values are limited to the following:

  - `"update"`: The normal mode: values already in `cache` are used
    as-is, anything missing is submitted to PubChem and the result added
    to the cache.

  - `"offline"`: Never submit a batch to PubChem - only values already
    present in `cache` are returned, and everything else is left as NA.
    Useful for continuing to work with a partially-populated cache while
    PubChem is unreachable or down, without waiting on or erroring
    against the live service. A warning lists how many query values are
    not covered by the cache when this happens (and, if the cache is
    entirely empty or unset, that every value will be returned as NA).

  - `"rebuild"`: Ignore any existing cached value and submit every value
    to PubChem, overwriting the corresponding entry in `cache`. Useful
    when cached results are known to be stale.

  The default is `"update"`.

- verbose:

  (logical) Whether to print debug information during execution. The
  default is `FALSE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `pubchem_id_exchange` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The annotation_source after adding data returned by the PUG API. |
| `query_batches` | (list) The query values submitted in each batch during training (excludes any already present in `cache`). |
| `query_values` | (character) Every unique, non-missing value of `query_column` seen during training (cached or not) - used to reconstruct the full result set in `model_predict`. |
| `request_ids` | (list) PubChem PUG request IDs for each submitted batch. |
| `trained` | (logical) Whether the model has already submitted its PubChem jobs. |

## Details

Ported from `structReportsPCB` (same authors), where it was developed to
drive PubChem's PUG XML ID Exchange service directly - a batch,
asynchronous submit/poll/download workflow, rather than the synchronous
per-row PUG REST calls used by
[`pubchem_compound_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/pubchem_compound_lookup.md)
/
[`pubchem_property_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/pubchem_property_lookup.md).
Prefer this object when translating a large number of identifiers at
once, since one request covers many query values instead of one request
per row.

## Inheritance

A `pubchem_id_exchange` object inherits the following `struct` classes:\
\
`[pubchem_id_exchange]` -\> `[model]` -\> `[struct_class]`

## See also

Other REST API's:
[`classyfire_batch_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/classyfire_batch_lookup.md),
[`classyfire_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/classyfire_lookup.md),
[`cts_lite_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/cts_lite_lookup.md),
[`kegg_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/kegg_lookup.md),
[`lipidmaps_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/lipidmaps_lookup.md),
[`mwb_compound_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_compound_lookup.md),
[`mwb_refmet_lookup()`](https://computational-metabolomics.github.io/MetMashR/reference/mwb_refmet_lookup.md),
[`rest_api()`](https://computational-metabolomics.github.io/MetMashR/reference/rest_api.md)

## Examples

``` r
M <- pubchem_id_exchange(
        query_column = character(0),
        output_type = "inchikey",
        suffix = "_pubchem_id_exchange",
        delay = 2,
        max_attempts = 30,
        max_poll_rounds = 10,
        input_type = "synonyms",
        input_source_name = NULL,
        output_source_name = NULL,
        n_batches = NULL,
        cache = NULL,
        cache_mode = "update",
        verbose = FALSE)
```
