# Is database writable

A function that returns TRUE if the database has been designed for use
in read and write mode.

## Usage

``` r
is_writable(obj, ...)

# S4 method for class 'annotation_database'
is_writable(obj)

# S4 method for class 'rdata_database'
is_writable(obj)
```

## Arguments

- obj:

  A `annotation_database` object

- ...:

  additional database specific inputs

## Value

TRUE if the database is writable; FALSE otherwise. This method does not
check file properties, only the intended usage of the object.

## Examples

``` r

M <- annotation_database()
is_writable(M)
#> [1] FALSE
```
