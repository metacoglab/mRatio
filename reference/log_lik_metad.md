# Generate a function to calculate the log likelihood of the metad' model

Generate a function to calculate the log likelihood of the metad' model

## Usage

``` r
log_lik_metad(i, prep)
```

## Arguments

- i:

  an observation index

- prep:

  an object containing the data and model draws

## Value

A `[D x K*4]` array containing posterior samples of the joint
probability of a type 1/type 2 response, where `D` is the number of
posterior draws, `N` is the number of rows in the data, and `K` is the
number of confidence levels.
