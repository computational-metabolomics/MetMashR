# Read a database

Reads an annotation_database and returns the data.frame.

## Usage

``` r
read_database(obj, ...)

# S4 method for class 'annotation_database'
read_database(obj)

# S4 method for class 'AnnotationDb_database'
read_database(obj)

# S4 method for class 'BiocFileCache_database'
read_database(obj)

# S4 method for class 'MTox700plus_database'
read_database(obj)

# S4 method for class 'PathBank_metabolite_database'
read_database(obj)

# S4 method for class 'excel_database'
read_database(obj)

# S4 method for class 'github_file'
read_database(obj)

# S4 method for class 'mwb_refmet_database'
read_database(obj)

# S4 method for class 'rdata_database'
read_database(obj)

# S4 method for class 'rds_database'
read_database(obj)

# S4 method for class 'sqlite_database'
read_database(obj)

# S4 method for class 'zenodo_file'
read_database(obj)
```

## Arguments

- obj:

  An `annotation_database` object

- ...:

  additional database specific inputs

## Value

A data.frame

## Examples

``` r
M <- rds_database(tempfile())
df <- read_database(M)
```
