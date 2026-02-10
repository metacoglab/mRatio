# Obtain posterior draws of the pseudo type 1 receiver operating characteristic (ROC) curve.

Obtain posterior draws of the pseudo type 1 receiver operating
characteristic (ROC) curve.

## Usage

``` r
roc1_draws(object, newdata, ..., bounds = FALSE)

add_roc1_draws(newdata, object, ...)
```

## Arguments

- object:

  The `brms` model with the `metad` family

- newdata:

  A data frame from which to generate posterior predictions

- ...:

  Additional parameters passed to
  [tidybayes::epred_draws](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html)

- bounds:

  If `TRUE`, include the endpoints of the ROC at \\(0, 0)\\ and \\(1,
  1)\\. Otherwise, the endpoints are excluded.

## Value

a tibble containing posterior draws of the pseudo type 1 ROC with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: identifiers for the posterior sample

- `joint_response`: the combined type 1 / type 2 response (\\J \in \[1,
  2K\]\\) for \\K\\ confidence levels)

- `response`: the type 1 response for perceived stimulus presence (\\R
  \in \\0, 1\\\\)

- `confidence`: the type 2 confidence response (\\C \in \[1, K\]\\)

- `p_fa`: the cumulative probability of a 'present'/'old' response for
  `stimulus==0` (\\P(J \ge j \\\vert\\ S=0)\\)

- `p_hit`: the cumulative probability of a 'present'/'old' response for
  `stimulus==1` (\\P(J \ge j \\\vert\\ S=1)\\)

## Examples

``` r
# running few iterations so example runs quickly, use more in practice
m <- fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 1.8e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.18 seconds.
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
#> Chain 1:  Elapsed Time: 0.036 seconds (Warm-up)
#> Chain 1:                0.029 seconds (Sampling)
#> Chain 1:                0.065 seconds (Total)
#> Chain 1: 
#> Warning: There were 1 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
newdata <- tidyr::tibble(.row = 1)

# compute pseudo-type 1 ROC curve
roc1_draws(m, newdata)
#> # A tibble: 1,750 × 9
#> # Groups:   .row, joint_response, response, confidence [7]
#>     .row .chain .iteration .draw joint_response response confidence  p_fa p_hit
#>    <int>  <int>      <int> <int>          <dbl>    <int>      <dbl> <dbl> <dbl>
#>  1     1     NA         NA     1              1        0          4 0.880 0.989
#>  2     1     NA         NA     2              1        0          4 0.920 0.988
#>  3     1     NA         NA     3              1        0          4 0.869 0.979
#>  4     1     NA         NA     4              1        0          4 0.858 0.992
#>  5     1     NA         NA     5              1        0          4 0.757 0.983
#>  6     1     NA         NA     6              1        0          4 0.861 0.984
#>  7     1     NA         NA     7              1        0          4 0.808 0.989
#>  8     1     NA         NA     8              1        0          4 0.872 0.947
#>  9     1     NA         NA     9              1        0          4 0.891 0.993
#> 10     1     NA         NA    10              1        0          4 0.855 0.974
#> # ℹ 1,740 more rows
add_roc1_draws(newdata, m)
#> # A tibble: 1,750 × 9
#> # Groups:   .row, joint_response, response, confidence [7]
#>     .row .chain .iteration .draw joint_response response confidence  p_fa p_hit
#>    <int>  <int>      <int> <int>          <dbl>    <int>      <dbl> <dbl> <dbl>
#>  1     1     NA         NA     1              1        0          4 0.880 0.989
#>  2     1     NA         NA     2              1        0          4 0.920 0.988
#>  3     1     NA         NA     3              1        0          4 0.869 0.979
#>  4     1     NA         NA     4              1        0          4 0.858 0.992
#>  5     1     NA         NA     5              1        0          4 0.757 0.983
#>  6     1     NA         NA     6              1        0          4 0.861 0.984
#>  7     1     NA         NA     7              1        0          4 0.808 0.989
#>  8     1     NA         NA     8              1        0          4 0.872 0.947
#>  9     1     NA         NA     9              1        0          4 0.891 0.993
#> 10     1     NA         NA    10              1        0          4 0.855 0.974
#> # ℹ 1,740 more rows

# include the ROC bounds
roc1_draws(m, newdata, bounds = TRUE)
#> # A tibble: 2,250 × 9
#> # Groups:   .row, joint_response, response, confidence [9]
#>     .row .chain .iteration .draw joint_response response confidence  p_fa p_hit
#>    <int>  <int>      <int> <int>          <dbl>    <dbl>      <dbl> <dbl> <dbl>
#>  1     1     NA         NA     1              0        0          5     1     1
#>  2     1     NA         NA     2              0        0          5     1     1
#>  3     1     NA         NA     3              0        0          5     1     1
#>  4     1     NA         NA     4              0        0          5     1     1
#>  5     1     NA         NA     5              0        0          5     1     1
#>  6     1     NA         NA     6              0        0          5     1     1
#>  7     1     NA         NA     7              0        0          5     1     1
#>  8     1     NA         NA     8              0        0          5     1     1
#>  9     1     NA         NA     9              0        0          5     1     1
#> 10     1     NA         NA    10              0        0          5     1     1
#> # ℹ 2,240 more rows
```
