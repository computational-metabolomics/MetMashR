# rt matching

Annotations will be matched to the measured variable meta data.frame by
determining which annotations rt window overlaps with the rt window from
the measured rt.

## Usage

``` r
rt_match(variable_meta, rt_column, rt_window, id_column, ...)
```

## Arguments

- variable_meta:

  (data.frame) A data.frame of variable IDs and their corresponding rt
  values.

- rt_column:

  (character) Column name of the rt values in variable_meta.

- rt_window:

  (numeric, integer) Rt window to use for matching. If a single value is
  provided then the same rt is used for both variable meta and the
  annotations. A named vector can also be provided e.g.
  c("variable_meta"=5,"annotations"=2) to use different ", "windows for
  each data table.

- id_column:

  (character) Column name of the variable ids in variable_meta. ",
  "id_column="rownames" will use the rownames as ids.

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A `rt_match` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_table) The input annotation source with the newly generated column. |

## Inheritance

A `rt_match` object inherits the following `struct` classes:\
\
`[rt_match]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- rt_match(
        variable_meta = data.frame(),
        rt_column = character(0),
        rt_window = 20,
        id_column = character(0))
```
