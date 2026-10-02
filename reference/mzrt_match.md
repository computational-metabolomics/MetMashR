# mz matching

Annotations will be matched to the measured data variable meta
data.frame by determining which annotations ppm AND rt windows overlap
with the ppm AND rt windows of the measured mz.

## Usage

``` r
mzrt_match(
  variable_meta,
  mz_column,
  rt_column,
  ppm_window,
  rt_window,
  id_column,
  ...
)
```

## Arguments

- variable_meta:

  (data.frame) A data.frame of variable IDs and their corresponding mz
  values.

- mz_column:

  (character) Column name of the mz values in variable_meta.

- rt_column:

  (character) Column name of the rt values in variable_meta.

- ppm_window:

  (numeric, integer) Ppm window to use for matching. If a single value
  is provided then the same ppm is used for both variable meta and the
  annotations. A named vector can also be provided e.g.
  c("variable_meta"=5,"annotations"=2) to use different ", "windows for
  each data table.

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

A `mzrt_match` object with the following `output` slots:

|  |  |
|----|----|
| `updated` | (annotation_source) The input annotation source with the newly generated column. |

## Inheritance

A `mzrt_match` object inherits the following `struct` classes:\
\
`[mzrt_match]` -\> `[model]` -\> `[struct_class]`

## Examples

``` r
M <- mzrt_match(
        variable_meta = data.frame(),
        mz_column = character(0),
        ppm_window = 5,
        id_column = character(0),
        rt_column = character(0),
        rt_window = 20)
```
