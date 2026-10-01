# Keep unique_records

reduces an annotation source to unique records only; all duplicates are
removed.

## Usage

``` r
unique_records(...)
```

## Arguments

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `unique_records` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The updated annotations as an `annotation_source` object. |

## Inheritance

A `unique_records` object inherits the following `struct` classes:\
\
`[unique_records]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- unique_records()
```
