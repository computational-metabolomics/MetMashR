# Write to a database

Writes a data.frame to a `annotation_database`.

## Usage

``` r
write_database(obj, ...)

# S4 method for class 'annotation_database'
write_database(obj, df)

# S4 method for class 'rds_database'
write_database(obj, df)

# S4 method for class 'sqlite_database'
write_database(obj, df)
```

## Arguments

- obj:

  A `annotation_database` object

- ...:

  additional database specific inputs

- df:

  (data.frame) the data.frame to store in the database.

## Value

Silently returns TRUE if successful, FALSE otherwise

## Examples

``` r
M <- rds_database(tempfile())
write_database(M, data.frame())
```
