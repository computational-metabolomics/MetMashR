# Trim whitespace

A wrapper for [`trimws()`](https://rdrr.io/r/base/trimws.html). Removes
leading and/or trailing whitespace from character strings.

## Usage

``` r
trim_whitespace(column_names, which = "both", whitespace = "[ \t\r\n]", ...)
```

## Arguments

- column_names:

  (character) The column name(s) in the annotation_source to trim white
  space from. Special case ".all" will apply to all columns.

- which:

  (character) Trailing and/or leading whitespace. Allowed values are
  limited to the following:

  - `""`: A character string specifying the location of whitespace to
    remove.

  - `"left"`: Remove leading whitespace.

  - `"right"`: Remove trailing whitespace.

  - `"both"`: Remove both leading and trailing whitespace.

  The default is `"both"`.

- whitespace:

  (character) A string specifying a regular expression to match (one
  character of) "white space". See
  [`trimws()`](https://rdrr.io/r/base/trimws.html) for details. The
  default is `"[ ]"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `trim_whitespace` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The annotation_source after trimming whitespace. |

## Inheritance

A `trim_whitespace` object inherits the following `struct` classes:\
\
`[trim_whitespace]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- trim_whitespace(
        column_names = "V1",
        which = "both",
        whitespace = "[   
]")
```
