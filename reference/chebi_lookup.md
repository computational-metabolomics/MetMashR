# ID/synonym lookup via ChEBI

Uses the ChEBI REST API to look up a ChEBI identifier from a compound
name/synonym, or a synonym from a ChEBI identifier, based on the input
annotation column.

## Usage

``` r
chebi_lookup(
  query_column,
  search_by = c("name", "chebi_id"),
  suffix = "_chebi",
  records = "best",
  max_records = "50",
  columns = ".all",
  delay = 1,
  ...
)
```

## Arguments

- query_column:

  (character) The name of a column in the annotation table containing
  the search terms. If search_by = "name" this should be a column of
  compound names/synonyms. If search_by = "chebi_id" this should be a
  column of ChEBI identifiers (e.g. "CHEBI:27732" or "27732"; any
  "CHEBI:" prefix is stripped automatically before querying the API).

- search_by:

  (character) Search by. Allowed values are limited to the following:

  - `"name"`: Search ChEBI by compound name/synonym and return the
    matching ChEBI ID(s).

  - `"chebi_id"`: Search ChEBI by ChEBI ID and return the matching
    synonym(s)/name(s).

  The default is `c("name", "chebi_id")`.

- suffix:

  (character) A suffix appended to all column names in the returned
  result. The default is `"_chebi"`.

- records:

  (character) Returned record(s). Allowed values are limited to the
  following:

  - `""`: There can be multiple matches for a given search term
    (multiple ChEBI entities matching a name, or multiple synonyms for a
    ChEBI ID).

  - `"best"`: Return only the single best/primary matching record.

  - `"all"`: Return all matching records.

  The default is `"best"`.

- max_records:

  (character) The maximum number of hits requested from the ChEBI search
  API when search_by = "name" (passed as the "size" query parameter).
  Has no effect when search_by = "chebi_id". The default is `"50"`.

- columns:

  (character) The columns to include in the result. One or more of
  "chebi_id", "name" (search_by = "name" only), "synonym" (search_by =
  "chebi_id" only), "inchikey", "smiles", "inchi", "formula", "mass",
  "monoisotopicmass", "charge", "stars" (ChEBI's own curation-quality
  rating, 3 = fully manually annotated). Keyword ".all" (the default)
  returns every column available for the chosen search_by direction. The
  default is `".all"`.

- delay:

  (numeric, integer) Delay in seconds between API calls. ChEBI's REST
  API does not publish a specific rate limit, so this default is a
  conservative courtesy value; increase it if you see 429/503 responses,
  or decrease it if you have confirmed a higher rate is acceptable. The
  default is `1`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `chebi_lookup` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The annotation_source after adding data returned by the API. |

## Inheritance

A `chebi_lookup` object inherits the following `struct` classes:\
\
`[chebi_lookup]` -\> `[rest_api]` -\> `[model]` -\> `[struct_class]`

## References

Hastings, Janna, Owen, Gareth, Dekker, Adriano, Ennis, Marcus, Kale,
Namrata, Muthukrishnan, Venkatesh, Turner, Steve, Swainston, Neil,
Mendes, Pedro, Steinbeck, Christoph (2016). "ChEBI in 2016: Improved
services and an expanding collection of metabolites." *Nucleic Acids
Research*, *44*(D1), D1214-D1219. doi:10.1093/nar/gkv1031
<https://doi.org/10.1093/nar/gkv1031>.

## Examples

``` r
M <- chebi_lookup(
        search_by = "name",
        records = "best",
        max_records = "50",
        columns = ".all",
        base_url = "https://www.ebi.ac.uk/chebi/backend/api/public",
        url_template = "<base_url>/es_search?term=<query_column>&size=<max_records>",
        query_column = character(0),
        cache = NULL,
        cache_mode = "update",
        status_codes = list(),
        delay = 1,
        suffix = "_rest_api")
```
