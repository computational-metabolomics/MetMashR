# Normalise Lipids nomenclature

Normalises differently formated lipid names to a consistent format.

## Usage

``` r
normalise_lipids(
  column_name,
  grammar = ".all",
  columns = ".all",
  suffix = "_goslin",
  batch_size = 10000,
  ...
)
```

## Arguments

- column_name:

  (character) The name of the column containing Lipids names to
  normalise.

- grammar:

  (character) The grammar to use for normalising lipid names. Allowed
  values are: Shorthand2020, Goslin, FattyAcids, LipidMaps, SwissLipids,
  HMDB or .all. The default is `".all"`.

- columns:

  (character) Column names to include from the goslin output. Can be any
  of "Normalized.Name", "Original.Name", "Grammar", "Adduct",
  "Adduct.Charge", "Lipid.Maps.Category", "Lipid.Maps.Main.Class",
  "Species.Name", "Extended.Species.Name", "Molecular.Species.Name",
  "Sn.Position.Name", "Structure.Defined.Name", "Full.Structure.Name",
  "Functional.Class.Abbr", "Functional.Class.Synonyms", "Level",
  "Total.C", "Total.OH", "Total.O", "Total.DB", "Mass",
  "Sum.Formula".".all" will return all columns. ". The default is
  `".all"`.

- suffix:

  (character) A suffic added to the column names of the goslin output.
  The default is `"_goslin"`.

- batch_size:

  (numeric, integer) The maximum number of annotations to be parsed by
  rgoslin at a time. If the batch size is less than the total number of
  records then the records will be split into multiple batches to help
  prevent crashes. The default is `10000`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `normalise_lipids` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) Annotation_source after normalising lipid names. |

## Details

This object makes use of functionality from the following packages:

- `rgoslin`

## Inheritance

A `normalise_lipids` object inherits the following `struct` classes:\
\
`[normalise_lipids]` -\> `[model]` -\> `[struct_class]`

## References

Kopczynski D, Hoffmann N, Peng B, Ahrends R (2020). "Goslin: A Grammar
of Succinct Lipid Nomenclature." *Analytical Chemistry*, *92*(16),
10957-10960. <https://pubs.acs.org/doi/10.1021/acs.analchem.0c01690>.

## Examples

``` r
M <- normalise_lipids(
        column_name = "V1",
        grammar = ".all",
        columns = ".all",
        suffix = "_goslin",
        batch_size = 10000)
```
