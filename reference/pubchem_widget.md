# PubChem widget

Display a PubChem HTML widget for a compound.

## Usage

``` r
pubchem_widget(
  query_column,
  row_index,
  record_type = "2D-Structure",
  hide_title = FALSE,
  width = "600px",
  height = "650px",
  display = TRUE,
  ...
)
```

## Arguments

- query_column:

  (character) The name of the `annotation_source` column with compound
  identifiers of the type specified in the `search_by` param.

- row_index:

  (integer, numeric) The row index of the `annotation_source` to request
  an image of the molecular structure of.

- record_type:

  (character) The record type for the widget. The default is
  `"2D-Structure"`.

- hide_title:

  (logical) Hide widget title. Allowed values are limited to the
  following:

  - `"TRUE"`: The title is displayed.

  - `"FALSE"`: The title is not displayed.

  The default is `FALSE`.\

- width:

  (integer, numeric, character) The width of the widget in a CSS style
  compatible format. Numerical values will be converted to character.
  The default is `"600px"`.

- height:

  (integer, numeric, character) The height of the widget in a CSS style
  compatible format.Numerical values will be converted to character. The
  default is `"650px"`.

- display:

  (logical) Display widget. Allowed values are limited to the following:

  - `"TRUE"`: Display the widget.

  - `"FALSE"`: Do not display the widget and only return the HTML.

  The default is `TRUE`.\

- ...:

  Additional slots and values passed to `struct_class`.

## Value

A ` pubchem_widget ` object. This object has no `output` slots. See
[`chart_plot`](https://rdrr.io/pkg/struct/man/chart_plot.html) in the
`struct` package to plot this chart object.

## Details

This object makes use of functionality from the following packages:

- `htmltools`

## Inheritance

A `pubchem_widget` object inherits the following `struct` classes:\
\
`[pubchem_widget]` -\> `[chart]` -\> `[struct_class]`

## References

Cheng J, Sievert C, Schloerke B, Chang W, Xie Y, Allen J (2025).
*htmltools: Tools for HTML*. doi:10.32614/CRAN.package.htmltools
<https://doi.org/10.32614/CRAN.package.htmltools>. R package version
0.5.9, <https://CRAN.R-project.org/package=htmltools>.

## Examples

``` r
M <- pubchem_widget(
        query_column = "V1",
        row_index = 1,
        record_type = "2D-Structure",
        hide_title = FALSE,
        width = 600,
        height = 400,
        display = FALSE)
```
