# Generate posterior predictions for the metad' model

Generate posterior predictions for the metad' model

## Usage

``` r
posterior_epred_metad(prep)
```

## Arguments

- prep:

  an object containing the data and model draws

## Value

A `[D x N x K*4]` array containing posterior samples of the joint
probability of a type 1/type 2 response, where `D` is the number of
posterior draws, `N` is the number of rows in the data, and `K` is the
number of confidence levels.
