# Simulate posterior predictions from the metad' model

Simulate posterior predictions from the metad' model

## Usage

``` r
posterior_predict_metad(i, prep, ...)
```

## Arguments

- i:

  an observation index

- prep:

  an object containing the data and model draws

- ...:

  Additional arguments. Not currently used.

## Value

A `[D x K*4]` array containing posterior samples of counts of joint type
1/type 2 responses, where `D` is the number of posterior draws, `N` is
the number of rows in the data, and `K` is the number of confidence
levels.
