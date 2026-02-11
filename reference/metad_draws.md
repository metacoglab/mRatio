# Obtain posterior draws of meta-d' model parameters

Given a data frame and a meta-d' model, adds estimates of all model
parameters. For `metad_draws` and `add_metad_draws`, parameters are
returned in a tidy tibble with one row per posterior draw. For
`metad_rvars` and `add_metad_rvars`, parameters are returned as
[posterior::rvar](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata`.

## Usage

``` r
metad_draws(object, newdata, ..., pivot_longer = FALSE)

add_metad_draws(newdata, object, ..., pivot_longer = FALSE)

metad_rvars(object, newdata, ..., pivot_longer = FALSE)

add_metad_rvars(newdata, object, pivot_longer = FALSE)
```

## Arguments

- object:

  The `brms` model with the `metad` family

- newdata:

  A data frame from which to generate posterior predictions

- ...:

  Additional arguments passed to
  [tidybayes::add_linpred_draws](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html)
  or
  [tidybayes::add_linpred_rvars](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

- pivot_longer:

  Return the draws in long format?

  - if `TRUE`, resulting data frame has one row per posterior draw per
    model parameter

  - if `FALSE` (default), resulting data frame has one row per posterior
    draw

## Value

a tibble containing posterior draws of model parameters with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `metad_draws`, identifiers for
  the posterior sample

- `.variable`, `.value`: if `pivot_longer=TRUE`, `.variable` identifies
  different meta-d' model parameters and `.value` stores posterior
  samples

- `M`, `dprime`, `c`, `meta_dprime`, `meta_c`, `meta_c2_0_<k>`,
  `meta_c2_1_<k>`: if `pivot_longer=FALSE`, posterior samples of all
  meta-d' model parameters

## Examples

``` r
# running few iterations so example runs quickly, use more in practice
m <- fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 2e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.2 seconds.
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
#> Chain 1:  Elapsed Time: 0.032 seconds (Warm-up)
#> Chain 1:                0.027 seconds (Sampling)
#> Chain 1:                0.059 seconds (Total)
#> Chain 1: 
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
newdata <- tidyr::tibble(.row = 1)

# obtain model parameters (wide format)
metad_draws(m, newdata)
#> # A tibble: 250 × 15
#> # Groups:   .row [1]
#>     .row .chain .iteration .draw      M dprime       c meta_dprime  meta_c
#>    <int>  <int>      <int> <int>  <dbl>  <dbl>   <dbl>       <dbl>   <dbl>
#>  1     1     NA         NA     1 0.0779  1.25  -0.0694      0.0974 -0.0694
#>  2     1     NA         NA     2 0.0955  1.32  -0.0563      0.126  -0.0694
#>  3     1     NA         NA     3 0.328   1.53  -0.0775      0.501  -0.0694
#>  4     1     NA         NA     4 1.21    0.589  0.290       0.712  -0.0694
#>  5     1     NA         NA     5 1.35    0.634  0.280       0.856  -0.0694
#>  6     1     NA         NA     6 1.36    0.872 -0.173       1.18   -0.0694
#>  7     1     NA         NA     7 1.49    1.07  -0.0221      1.60   -0.0694
#>  8     1     NA         NA     8 0.828   1.27   0.0909      1.05   -0.0694
#>  9     1     NA         NA     9 0.169   0.885  0.0355      0.150  -0.0694
#> 10     1     NA         NA    10 0.0667  0.636 -0.168       0.0424 -0.0694
#> # ℹ 240 more rows
#> # ℹ 6 more variables: meta_c2_0_1 <dbl>, meta_c2_0_2 <dbl>, meta_c2_0_3 <dbl>,
#> #   meta_c2_1_1 <dbl>, meta_c2_1_2 <dbl>, meta_c2_1_3 <dbl>
add_metad_draws(newdata, m)
#> # A tibble: 250 × 15
#> # Groups:   .row [1]
#>     .row .chain .iteration .draw      M dprime       c meta_dprime  meta_c
#>    <int>  <int>      <int> <int>  <dbl>  <dbl>   <dbl>       <dbl>   <dbl>
#>  1     1     NA         NA     1 0.0779  1.25  -0.0694      0.0974 -0.0694
#>  2     1     NA         NA     2 0.0955  1.32  -0.0563      0.126  -0.0694
#>  3     1     NA         NA     3 0.328   1.53  -0.0775      0.501  -0.0694
#>  4     1     NA         NA     4 1.21    0.589  0.290       0.712  -0.0694
#>  5     1     NA         NA     5 1.35    0.634  0.280       0.856  -0.0694
#>  6     1     NA         NA     6 1.36    0.872 -0.173       1.18   -0.0694
#>  7     1     NA         NA     7 1.49    1.07  -0.0221      1.60   -0.0694
#>  8     1     NA         NA     8 0.828   1.27   0.0909      1.05   -0.0694
#>  9     1     NA         NA     9 0.169   0.885  0.0355      0.150  -0.0694
#> 10     1     NA         NA    10 0.0667  0.636 -0.168       0.0424 -0.0694
#> # ℹ 240 more rows
#> # ℹ 6 more variables: meta_c2_0_1 <dbl>, meta_c2_0_2 <dbl>, meta_c2_0_3 <dbl>,
#> #   meta_c2_1_1 <dbl>, meta_c2_1_2 <dbl>, meta_c2_1_3 <dbl>

# obtain model parameters (long format)
metad_draws(m, newdata, pivot_longer = TRUE)
#> # A tibble: 2,750 × 6
#> # Groups:   .row, .variable [11]
#>     .row .chain .iteration .draw .variable    .value
#>    <int>  <int>      <int> <int> <chr>         <dbl>
#>  1     1     NA         NA     1 M            0.0779
#>  2     1     NA         NA     1 dprime       1.25  
#>  3     1     NA         NA     1 c           -0.0694
#>  4     1     NA         NA     1 meta_dprime  0.0974
#>  5     1     NA         NA     1 meta_c      -0.0694
#>  6     1     NA         NA     1 meta_c2_0_1 -0.480 
#>  7     1     NA         NA     1 meta_c2_0_2 -0.817 
#>  8     1     NA         NA     1 meta_c2_0_3 -1.02  
#>  9     1     NA         NA     1 meta_c2_1_1  0.350 
#> 10     1     NA         NA     1 meta_c2_1_2  0.684 
#> # ℹ 2,740 more rows
add_metad_draws(newdata, m, pivot_longer = TRUE)
#> # A tibble: 2,750 × 6
#> # Groups:   .row, .variable [11]
#>     .row .chain .iteration .draw .variable    .value
#>    <int>  <int>      <int> <int> <chr>         <dbl>
#>  1     1     NA         NA     1 M            0.0779
#>  2     1     NA         NA     1 dprime       1.25  
#>  3     1     NA         NA     1 c           -0.0694
#>  4     1     NA         NA     1 meta_dprime  0.0974
#>  5     1     NA         NA     1 meta_c      -0.0694
#>  6     1     NA         NA     1 meta_c2_0_1 -0.480 
#>  7     1     NA         NA     1 meta_c2_0_2 -0.817 
#>  8     1     NA         NA     1 meta_c2_0_3 -1.02  
#>  9     1     NA         NA     1 meta_c2_1_1  0.350 
#> 10     1     NA         NA     1 meta_c2_1_2  0.684 
#> # ℹ 2,740 more rows

# obtain model parameters (wide format, posterior::rvar)
metad_rvars(m, newdata)
#> # A tibble: 1 × 12
#> # Groups:   .row [1]
#>    .row           M     dprime              c meta_dprime         meta_c
#>   <dbl>  <rvar[1d]> <rvar[1d]>     <rvar[1d]>  <rvar[1d]>     <rvar[1d]>
#> 1     1  0.92 ± 0.5   1 ± 0.27  0.0068 ± 0.12  0.9 ± 0.41  0.0068 ± 0.12
#> # ℹ 6 more variables: meta_c2_0_1 <rvar[1d]>, meta_c2_0_2 <rvar[1d]>,
#> #   meta_c2_0_3 <rvar[1d]>, meta_c2_1_1 <rvar[1d]>, meta_c2_1_2 <rvar[1d]>,
#> #   meta_c2_1_3 <rvar[1d]>
add_metad_rvars(newdata, m)
#> # A tibble: 1 × 12
#> # Groups:   .row [1]
#>    .row           M     dprime              c meta_dprime         meta_c
#>   <dbl>  <rvar[1d]> <rvar[1d]>     <rvar[1d]>  <rvar[1d]>     <rvar[1d]>
#> 1     1  0.92 ± 0.5   1 ± 0.27  0.0068 ± 0.12  0.9 ± 0.41  0.0068 ± 0.12
#> # ℹ 6 more variables: meta_c2_0_1 <rvar[1d]>, meta_c2_0_2 <rvar[1d]>,
#> #   meta_c2_0_3 <rvar[1d]>, meta_c2_1_1 <rvar[1d]>, meta_c2_1_2 <rvar[1d]>,
#> #   meta_c2_1_3 <rvar[1d]>

# obtain model parameters (long format, posterior::rvar)
metad_rvars(m, newdata, pivot_longer = TRUE)
#> # A tibble: 11 × 3
#> # Groups:   .row, .variable [11]
#>     .row .variable            .value
#>    <dbl> <chr>            <rvar[1d]>
#>  1     1 M             0.9242 ± 0.50
#>  2     1 dprime        1.0432 ± 0.27
#>  3     1 c             0.0068 ± 0.12
#>  4     1 meta_dprime   0.9021 ± 0.41
#>  5     1 meta_c        0.0068 ± 0.12
#>  6     1 meta_c2_0_1  -0.5226 ± 0.14
#>  7     1 meta_c2_0_2  -0.9470 ± 0.16
#>  8     1 meta_c2_0_3  -1.2075 ± 0.17
#>  9     1 meta_c2_1_1   0.6261 ± 0.14
#> 10     1 meta_c2_1_2   0.9326 ± 0.16
#> 11     1 meta_c2_1_3   1.4374 ± 0.20
add_metad_rvars(newdata, m, pivot_longer = TRUE)
#> # A tibble: 11 × 3
#> # Groups:   .row, .variable [11]
#>     .row .variable            .value
#>    <dbl> <chr>            <rvar[1d]>
#>  1     1 M             0.9242 ± 0.50
#>  2     1 dprime        1.0432 ± 0.27
#>  3     1 c             0.0068 ± 0.12
#>  4     1 meta_dprime   0.9021 ± 0.41
#>  5     1 meta_c        0.0068 ± 0.12
#>  6     1 meta_c2_0_1  -0.5226 ± 0.14
#>  7     1 meta_c2_0_2  -0.9470 ± 0.16
#>  8     1 meta_c2_0_3  -1.2075 ± 0.17
#>  9     1 meta_c2_1_1   0.6261 ± 0.14
#> 10     1 meta_c2_1_2   0.9326 ± 0.16
#> 11     1 meta_c2_1_3   1.4374 ± 0.20
```
