# Normalise string

Replace matching (sub)strings based on a provided dictionary of search
terms and their replacements.

## Usage

``` r
normalise_strings(
  search_column,
  output_column = NULL,
  dictionary = list(),
  ...
)
```

## Arguments

- search_column:

  (character) The column name of the input `annotation_source` that will
  be searched for matching (sub)strings.

- output_column:

  (character, NULL) The name of a new column that the modified strings
  will be stored in. If NULL the `search_column` will be replaced. The
  default is `NULL`.

- dictionary:

  (list, annotation_database) A list of patterns and functions that take
  the input pattern and return a replacement string. A
  `annotation_database` object containing a suitable list can also be
  used here. The default is
  [`list()`](https://rdrr.io/r/base/list.html).

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `normalise_strings` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The updated annotations as an `annotation_source` object. |

## Details

This object makes use of functionality from the following packages:

- `dplyr`

Each item of the `dictionary` list should \#' have at least two fields:
"pattern" and "replace". "pattern" is used as inputs to the `[grepl()]`
function to detect matches to the input pattern. Parameters such as
`perl = TRUE` can also be included in the list and these will be passed
to `[grepl()]`, otherwise the defaults are used. When a match is
detected the function in "replace" is called with the same inputs as
`[grepl()]`. The "replace" function should return a new string.
Alternatively `replace = NA` can be used to return NA for a matching
pattern. If a character string is provided then `[gsub()]` will be used
by default.

## Inheritance

A `normalise_strings` object inherits the following `struct` classes:\
\
`[normalise_strings]` -\> `[model]` -\> `[struct_class]`

## References

Wickham H, François R, Henry L, Müller K, Vaughan D (2026). *dplyr: A
Grammar of Data Manipulation*. doi:10.32614/CRAN.package.dplyr
<https://doi.org/10.32614/CRAN.package.dplyr>. R package version 1.2.1,
<https://CRAN.R-project.org/package=dplyr>.

## See also

[`grepl()`](https://rdrr.io/r/base/grep.html),
[`gsub()`](https://rdrr.io/r/base/grep.html)

## Examples

``` r
M <- normalise_strings(
        search_column = character(0),
        output_column = NULL,
        dictionary = list())
```
