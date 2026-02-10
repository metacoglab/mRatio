# Obtain posterior draws of the response-specific type 2 receiver operating characteristic (ROC) curves.

Obtain posterior draws of the response-specific type 2 receiver
operating characteristic (ROC) curves.

## Usage

``` r
roc2_draws(object, newdata, ..., bounds = FALSE)

add_roc2_draws(newdata, object, ...)
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

- `p_fa2`: the cumulative probability of an incorrect response (\\P(C\ge
  c \\\vert\\ R\ne S)\\)

- `p_hit2`: the cumulative probability of a correct response (\\P(C\ge c
  \\\vert\\ R = S)\\)

## Examples

``` r
# running few iterations so example runs quickly, use more in practice
m <- fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 1.7e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.17 seconds.
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
#> Chain 1:  Elapsed Time: 0.028 seconds (Warm-up)
#> Chain 1:                0.026 seconds (Sampling)
#> Chain 1:                0.054 seconds (Total)
#> Chain 1: 
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
newdata <- tidyr::tibble(.row = 1)

# compute type 2 ROC curve
roc2_draws(m, newdata)
#> # A tibble: 1,500 × 8
#> # Groups:   .row, response, confidence [6]
#>     .row .chain .iteration .draw response confidence p_hit2  p_fa2
#>    <int>  <int>      <int> <int>    <int>      <dbl>  <dbl>  <dbl>
#>  1     1     NA         NA     1        0          4 0.294  0.135 
#>  2     1     NA         NA     2        0          4 0.179  0.0240
#>  3     1     NA         NA     3        0          4 0.281  0.198 
#>  4     1     NA         NA     4        0          4 0.0620 0.0434
#>  5     1     NA         NA     5        0          4 0.271  0.203 
#>  6     1     NA         NA     6        0          4 0.256  0.208 
#>  7     1     NA         NA     7        0          4 0.293  0.179 
#>  8     1     NA         NA     8        0          4 0.236  0.0916
#>  9     1     NA         NA     9        0          4 0.260  0.0558
#> 10     1     NA         NA    10        0          4 0.175  0.0715
#> # ℹ 1,490 more rows
add_roc2_draws(newdata, m)
#> # A tibble: 1,500 × 8
#> # Groups:   .row, response, confidence [6]
#>     .row .chain .iteration .draw response confidence p_hit2  p_fa2
#>    <int>  <int>      <int> <int>    <int>      <dbl>  <dbl>  <dbl>
#>  1     1     NA         NA     1        0          4 0.294  0.135 
#>  2     1     NA         NA     2        0          4 0.179  0.0240
#>  3     1     NA         NA     3        0          4 0.281  0.198 
#>  4     1     NA         NA     4        0          4 0.0620 0.0434
#>  5     1     NA         NA     5        0          4 0.271  0.203 
#>  6     1     NA         NA     6        0          4 0.256  0.208 
#>  7     1     NA         NA     7        0          4 0.293  0.179 
#>  8     1     NA         NA     8        0          4 0.236  0.0916
#>  9     1     NA         NA     9        0          4 0.260  0.0558
#> 10     1     NA         NA    10        0          4 0.175  0.0715
#> # ℹ 1,490 more rows

# include the ROC bounds
roc2_draws(m, newdata, bounds = TRUE)
#> # A tibble: 2,500 × 8
#> # Groups:   .row, response, confidence [10]
#>     .row .chain .iteration .draw response confidence p_hit2  p_fa2
#>    <int>  <int>      <int> <int>    <dbl>      <dbl>  <dbl>  <dbl>
#>  1     1     NA         NA     1        0          4 0.294  0.135 
#>  2     1     NA         NA     2        0          4 0.179  0.0240
#>  3     1     NA         NA     3        0          4 0.281  0.198 
#>  4     1     NA         NA     4        0          4 0.0620 0.0434
#>  5     1     NA         NA     5        0          4 0.271  0.203 
#>  6     1     NA         NA     6        0          4 0.256  0.208 
#>  7     1     NA         NA     7        0          4 0.293  0.179 
#>  8     1     NA         NA     8        0          4 0.236  0.0916
#>  9     1     NA         NA     9        0          4 0.260  0.0558
#> 10     1     NA         NA    10        0          4 0.175  0.0715
#> # ℹ 2,490 more rows
```
