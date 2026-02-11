# Fit the meta-d' model using `brms` package

This function is a wrapper around
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) using
a custom family for the meta-d' model.

## Usage

``` r
fit_metad(
  formula,
  data,
  ...,
  aggregate = TRUE,
  K = NULL,
  distribution = "normal",
  metac_absolute = TRUE,
  stanvars = NULL
)
```

## Arguments

- formula:

  A model formula for some or all parameters of the `metad` brms family.
  To display all parameter names for a model with `K` confidence levels,
  use `metad(K)`.

- data:

  A tibble containing the data to fit the model.

  - If `aggregate`==TRUE, `data` should have one row per observation
    with columns `stimulus`, `response`, `confidence`, and any other
    variables in `formula`

  - If `aggregate`==FALSE, it should be aggregated to have one row per
    cell of the design matrix, with joint type 1/type 2 response counts
    in a matrix column (see
    [`aggregate_metad()`](https://metacoglab.github.io/mRatio/reference/aggregate_metad.md)).

- ...:

  Additional parameters passed to the `brm` function.

- aggregate:

  If `TRUE`, automatically aggregate `data` by the variables included in
  `formula` using
  [`aggregate_metad()`](https://metacoglab.github.io/mRatio/reference/aggregate_metad.md).
  Otherwise, `data` should already be aggregated.

- K:

  The number of confidence levels. By default, this is estimated from
  the data.

- distribution:

  The noise distribution to use for the signal detection model. By
  default, uses a normal distribution with a mean parameterized by
  `dprime`.

- metac_absolute:

  If `TRUE`, fix the type 2 criterion to be equal to the type 1
  criterion. Otherwise, equate the criteria relatively such that
  metac/metadprime = c/dprime.

- stanvars:

  Additional `stanvars` to pass to the model code, for example to define
  an alternative distribution or a custom model prior (see
  [`brms::stanvar()`](https://paulbuerkner.com/brms/reference/stanvar.html)).

## Examples

``` r
# fit a basic model on simulated data
# running few iterations so example runs quickly, use more in practice
fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: Rejecting initial value:
#> Chain 1:   Gradient evaluated at the initial value is not finite.
#> Chain 1:   Stan can't start sampling from this initial value.
#> Chain 1: 
#> Chain 1: Gradient evaluation took 1e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.1 seconds.
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
#> Chain 1:  Elapsed Time: 0.027 seconds (Warm-up)
#> Chain 1:                0.023 seconds (Sampling)
#> Chain 1:                0.05 seconds (Total)
#> Chain 1: 
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
#>  Family: metad__4__normal__absolute 
#>   Links: mu = log 
#> Formula: N ~ 1 
#>    Data: data.aggregated (Number of observations: 1) 
#>   Draws: 1 chains, each with iter = 500; warmup = 250; thin = 1;
#>          total post-warmup draws = 250
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept    -0.41      0.88    -1.85     0.62 1.00      118       97
#> 
#> Further Distributional Parameters:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> dprime              1.19      0.27     0.63     1.73 1.01      288      193
#> c                   0.17      0.15    -0.14     0.44 1.01      164      151
#> metac2zero1diff     0.45      0.11     0.25     0.66 1.03      167      125
#> metac2zero2diff     0.69      0.12     0.48     0.94 1.01      216      112
#> metac2zero3diff     0.63      0.15     0.37     0.94 1.00      276      158
#> metac2one1diff      0.51      0.12     0.30     0.77 1.00      215      181
#> metac2one2diff      0.54      0.13     0.29     0.82 1.01      230      199
#> metac2one3diff      0.33      0.13     0.14     0.59 1.03      239      173
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```
