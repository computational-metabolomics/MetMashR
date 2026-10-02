# Apply method

Applies method to the input DatasetExperiment. Some models also provide
separate `model_train` and `model_predict` methods.

## Usage

``` r
model_apply(M, D, ...)
model_train(M, D, ...)
model_predict(M, D, ...)
```

## Arguments

- M:

  a method object

- D:

  another object used by the first

- ...:

  additional inputs (not used)

## Value

Returns a modified method object

## Examples

``` r
M <- example_model()
M <- model_apply(M, iris_DatasetExperiment())
```
