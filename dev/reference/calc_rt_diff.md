# Calculate RT difference

Calculate RT difference between two RT values

## Usage

``` r
calc_rt_diff(
  obs_rt_column,
  ref_rt_column,
  out_column,
  check_names = "unique",
  ...
)
```

## Arguments

- obs_rt_column:

  (character) Column name in annotation table containing the observed
  (measured) RT values.

- ref_rt_column:

  (character) Column name in annotation table containing the reference
  (theoretical) RT values.

- out_column:

  (character) Column name in annotation table to store the computed RT
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

A `calc_rt_diff` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_table) The input annotation source with the newly generated column. |

## Inheritance

A `calc_rt_diff` object inherits the following `struct` classes:\
\
`[calc_rt_diff]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- calc_rt_diff(
        obs_rt_column = character(0),
        ref_rt_column = character(0),
        out_column = character(0),
        check_names = "unique")
```
