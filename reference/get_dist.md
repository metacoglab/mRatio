# Get the R function for the model's underlying distribution functions

Get the R function for the model's underlying distribution functions

## Usage

``` r
get_dist(model, fun = "lcdf")
```

## Arguments

- model:

  The `brms` model to get distribution functions for

- fun:

  The distribution function to return.

## Value

An R function with the name `distribution_{fun}`.

## Details

Will throw an error if this function does not exist
