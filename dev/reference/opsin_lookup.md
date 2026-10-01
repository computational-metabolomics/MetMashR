# Compound ID lookup via OPSIN

Uses the [OPSIN API](https://opsin.ch.cam.ac.uk/) to search for
identifers based on the input annotation column.

## Usage

``` r
opsin_lookup(query_column, suffix = "_opsin", output = "cids", ...)
```

## Arguments

- query_column:

  (character) The column name to use as the reference for searching the
  database e.g. "compound_name". OPSIN expect molecule names as input.

- suffix:

  (character) A suffix appended to all column names in the returned
  result. The default is `"_opsin"`.

- output:

  (character) The value returned from the pubchem database. The default
  is `"cids"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `opsin_lookup` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The annotation_source after adding data returned by the API. |

## Inheritance

A `opsin_lookup` object inherits the following `struct` classes:\
\
`[opsin_lookup]` -\> `[rest_api]` -\> `[model]` -\> `[struct_class]`

## References

Lowe, M. D, Corbett, T. P, Murray-Rust, Peter, Glen, C. R (2011).
"Chemical Name to Structure: OPSIN, an Open ", "Source Solution."
*Journal of Chemical Information and Modeling*, *51*(3), 793-753.
doi:10.1021/ci100384d <https://doi.org/10.1021/ci100384d>.

## Examples

``` r
M <- opsin_lookup(
        output = "stdinchikey",
        base_url = "https://opsin.ch.cam.ac.uk/opsin",
        url_template = "<base_url>/<query_column>.<output>",
        query_column = character(0),
        cache = NULL,
        cache_mode = "update",
        status_codes = list(),
        delay = 0.5,
        suffix = "_rest_api")
```
