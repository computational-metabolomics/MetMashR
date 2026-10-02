# Calculate ppm difference

Calculate ppm difference between two columns in an
[`annotation_table`](https://computational-metabolomics.github.io/MetMashR/reference/annotation_table.md).
e.g. for comparing observed m/z to theortical ones.

## Usage

``` r
calc_ppm_diff(
  obs_mz_column,
  ref_mz_column,
  out_column,
  check_names = "unique",
  ...
)
```

## Arguments

- obs_mz_column:

  (character) Column name in annotation_table containing the observed
  m/z values.

- ref_mz_column:

  (character) Column name in annotation table containing the .

- out_column:

  (character) Column name in annotation table to store the computed ppm
  differences.

- check_names:

  (character) Check names. Allowed values are limited to the following:

  - `"stop"`: If the output column already exists an error will be
    thrown.

  - `"unique"`: If the output column already exists a unique column name
    will be generated.

  - `"replace"`: If the output column already exists it will be
    replaced.

  The default is `"unique"`.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `calc_ppm_diff` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_table) The input annotation source with the computed ppm diffences in a new column. |

## Inheritance

A `calc_ppm_diff` object inherits the following `struct` classes:\
\
`[calc_ppm_diff]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- calc_ppm_diff(
        obs_mz_column = character(0),
        ref_mz_column = "reference (theoretical) m/z values.",
        out_column = character(0),
        check_names = "unique")
```
