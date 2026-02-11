# Obtain posterior draws of an index of metacognitive bias

Computes \\\textrm{meta-}\Delta\\, an index of metacognitive bias.
\\\textrm{meta-}\Delta\\ is the distance between `meta_c` and the
average of the the confidence criteria `meta_c2_0` and `meta_c2_1`. For
`metacognitive_bias_draws` and `add_metacognitive_bias_draws`,
parameters are returned in a tidy tibble with one row per posterior draw
and per response. For `metacognitive_bias_rvars` and
`add_metacognitive_bias_rvars`, parameters are returned as
[posterior::rvar](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata` and per response.

## Usage

``` r
metacognitive_bias_draws(object, newdata, ..., by_response = TRUE)

add_metacognitive_bias_draws(newdata, object, ...)

metacognitive_bias_rvars(object, newdata, ..., by_response = TRUE)

add_metacognitive_bias_rvars(newdata, object, ...)
```

## Arguments

- object:

  The `brms` model with the `metad` family

- newdata:

  A data frame from which to generate posterior predictions

- ...:

  Additional parameters passed to
  [tidybayes::epred_draws](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html)
  or
  [tidybayes::epred_rvars](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

- by_response:

  If `TRUE`, compute metacognitive bias separately for the two type 1
  responses. If `FALSE`, compute an un-weighted average of the two
  measures.

## Value

a tibble containing posterior draws of \\\textrm{meta-}\Delta\\ with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `metacognitive_bias_draws` and
  `add_metacognitive_bias_draws`, identifiers for the posterior sample

- `response`: the type 1 response for perceived stimulus presence

- `metacognitive_bias`: the distance between `meta_c` and the average of
  the confidence criteria `meta_c2_{response}`.

## Examples

``` r
# running few iterations so example runs quickly, use more in practice
m <- fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 2.4e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.24 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:   1 / 500 [  0%]  (Warmup)
#> Chain 1: Iteration:  50 / 500 [ 10%]  (Warmup)
#> Chain 1: Iteration: 100 / 500 [ 20%]  (Warmup)
#> Chain 1: Iteration: 150 / 500 [ 30%]  (Warmup)
#> Chain 1: Iteration: 200 / 500 [ 40%]  (Warmup)
#> Chain 1: Iteration: 250 / 500 [ 50%]  (Warmup)
#> Chain 1: Iteration: 251 / 500 [ 50%]  (Sampling)
#> Chain 1: Iteration: 300 / 500 [ 60%]  (Sampling)
#> Chain 1: Iteration: 350 / 500 [ 70%]  (Sampling)
#> Chain 1: Iteration: 400 / 500 [ 80%]  (Sampling)
#> Chain 1: Iteration: 450 / 500 [ 90%]  (Sampling)
#> Chain 1: Iteration: 500 / 500 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.03 seconds (Warm-up)
#> Chain 1:                0.025 seconds (Sampling)
#> Chain 1:                0.055 seconds (Total)
#> Chain 1: 
newdata <- tidyr::tibble(.row = 1)

# compute metacognitive bias
metacognitive_bias_draws(m, newdata)
#> # A tibble: 500 × 6
#> # Groups:   .row, response [2]
#>     .row response .chain .iteration .draw metacognitive_bias
#>    <int>    <int>  <int>      <int> <int>              <dbl>
#>  1     1        0     NA         NA     1              0.981
#>  2     1        0     NA         NA     2              1.18 
#>  3     1        0     NA         NA     3              1.10 
#>  4     1        0     NA         NA     4              1.00 
#>  5     1        0     NA         NA     5              0.875
#>  6     1        0     NA         NA     6              1.04 
#>  7     1        0     NA         NA     7              1.01 
#>  8     1        0     NA         NA     8              1.14 
#>  9     1        0     NA         NA     9              1.04 
#> 10     1        0     NA         NA    10              1.06 
#> # ℹ 490 more rows
add_metacognitive_bias_draws(newdata, m)
#> # A tibble: 500 × 6
#> # Groups:   .row, response [2]
#>     .row response .chain .iteration .draw metacognitive_bias
#>    <int>    <int>  <int>      <int> <int>              <dbl>
#>  1     1        0     NA         NA     1              0.981
#>  2     1        0     NA         NA     2              1.18 
#>  3     1        0     NA         NA     3              1.10 
#>  4     1        0     NA         NA     4              1.00 
#>  5     1        0     NA         NA     5              0.875
#>  6     1        0     NA         NA     6              1.04 
#>  7     1        0     NA         NA     7              1.01 
#>  8     1        0     NA         NA     8              1.14 
#>  9     1        0     NA         NA     9              1.04 
#> 10     1        0     NA         NA    10              1.06 
#> # ℹ 490 more rows

# use posterior::rvar for increased efficiency
metacognitive_bias_rvars(m, newdata)
#> # A tibble: 2 × 3
#> # Groups:   .row, response [2]
#>    .row response metacognitive_bias
#>   <dbl>    <int>         <rvar[1d]>
#> 1     1        0        1.02 ± 0.13
#> 2     1        1        0.97 ± 0.14
add_metacognitive_bias_rvars(newdata, m)
#> # A tibble: 2 × 3
#> # Groups:   .row, response [2]
#>    .row response metacognitive_bias
#>   <dbl>    <int>         <rvar[1d]>
#> 1     1        0        1.02 ± 0.13
#> 2     1        1        0.97 ± 0.14

# average over the two type 1 responses
metacognitive_bias_draws(m, newdata, by_response = FALSE)
#> # A tibble: 250 × 5
#> # Groups:   .row [1]
#>     .row .chain .iteration .draw metacognitive_bias
#>    <int>  <int>      <int> <int>              <dbl>
#>  1     1     NA         NA     1              1.00 
#>  2     1     NA         NA     2              1.04 
#>  3     1     NA         NA     3              0.983
#>  4     1     NA         NA     4              0.927
#>  5     1     NA         NA     5              1.01 
#>  6     1     NA         NA     6              1.07 
#>  7     1     NA         NA     7              0.882
#>  8     1     NA         NA     8              1.00 
#>  9     1     NA         NA     9              1.03 
#> 10     1     NA         NA    10              1.14 
#> # ℹ 240 more rows
metacognitive_bias_rvars(m, newdata, by_response = FALSE)
#> # A tibble: 1 × 2
#>    .row metacognitive_bias
#>   <dbl>         <rvar[1d]>
#> 1     1       0.99 ± 0.098
```
