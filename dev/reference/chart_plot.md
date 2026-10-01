# chart_plot method

Plots a chart object

## Usage

``` r
# S4 method for class 'annotation_bar_chart,annotation_source'
chart_plot(obj, dobj)

# S4 method for class 'annotation_histogram,annotation_source'
chart_plot(obj, dobj)

# S4 method for class 'annotation_histogram2d,annotation_source'
chart_plot(obj, dobj)

# S4 method for class 'annotation_pie_chart,annotation_source'
chart_plot(obj, dobj)

# S4 method for class 'annotation_venn_chart,annotation_source'
chart_plot(obj, dobj, ...)

# S4 method for class 'annotation_venn_chart,list'
chart_plot(obj, dobj)

# S4 method for class 'annotation_upset_chart,annotation_source'
chart_plot(obj, dobj, ...)

# S4 method for class 'annotation_upset_chart,list'
chart_plot(obj, dobj)

# S4 method for class 'mwb_structure,annotation_source'
chart_plot(obj, dobj)

# S4 method for class 'pubchem_structure,annotation_source'
chart_plot(obj, dobj)

# S4 method for class 'pubchem_widget,annotation_source'
chart_plot(obj, dobj)
```

## Arguments

- obj:

  a chart object

- dobj:

  a struct object

- ...:

  additiional inputs to chart_plot

## Value

a plot object

## Examples

``` r
C <- example_chart()
chart_plot(C, example_model())

#> NULL
```
