# Import_source

A wrapper for
[`read_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/read_source.md)
that can be used in an annotation workflow to import an annotation
source.

## Usage

``` r
import_source(...)
```

## Arguments

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `import_source` object with the following `output` slots:

|  |  |
|----|----|
| `imported` | (annotation_source) The `annotation_source` after importing the data. |

## Inheritance

A `import_source` object inherits the following `struct` classes:\
\
`[import_source]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- import_source()
```
