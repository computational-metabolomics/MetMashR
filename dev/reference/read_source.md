# Import annotation source

Import an data from e.g. a raw file and parse it into an
[`annotation_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/annotation_source.md)
object.

## Usage

``` r
read_source(obj, ...)

# S4 method for class 'annotation_source'
read_source(obj)

# S4 method for class 'annotation_database'
read_source(obj)

# S4 method for class 'cd_source'
read_source(obj)

# S4 method for class 'ls_source'
read_source(obj)

# S4 method for class 'mspurity_source'
read_source(obj)

# S4 method for class 'mwb_study_source'
read_source(obj)
```

## Arguments

- obj:

  an
  [`annotation_source()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/annotation_source.md)
  object

- ...:

  not currently used

## Value

an
[`annotation_table()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/annotation_table.md)
or
[`annotation_database()`](https://computational-metabolomics.github.io/MetMashR/dev/reference/annotation_database.md)
object

## Examples

``` r
# prepare source
CD <- cd_source(
    source = system.file(
        paste0("extdata/MTox/CD/HILIC_POS.xlsx"),
        package = "MetMashR"
    )
)
```
