# Import CompDB source

Imports the compounds table of a CompDB source as an
`annotation_source`.

## Usage

``` r
CompoundDb_source(source, tag = "cdb", ...)
```

## Arguments

- source:

  (ANY) The source of annotation data.

- tag:

  (character) A (short) character string that is used to represent this
  source e.g. in column names or source columns when used in a workflow.
  The default is `"cdb"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` CompoundDb_source ` object. This object has no `output` slots.

## Details

This object makes use of functionality from the following packages:

- `CompoundDb`

## Inheritance

A `CompoundDb_source` object inherits the following `struct` classes:\
\
`[CompoundDb_source]` -\> `[annotation_source]` -\> `[struct_class]`

## References

Rainer J, Vicini A, Salzer L, Stanstrup J, Badia J, Neumann S, Stravs M,
Verri Hernandes V, Gatto L, Gibb S, Witting M (2022). "A Modular and
Expandable Ecosystem for Metabolomics Data Annotation in R."
*Metabolites*, *12*, 173. doi:10.3390/metabo12020173
<https://doi.org/10.3390/metabo12020173>.
<https://www.mdpi.com/2218-1989/12/2/173>.

## Examples

``` r
M <- CompoundDb_source(
        tag = character(0),
        data = data.frame(),
        source = "ANY")
```
