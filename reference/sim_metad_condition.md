# Simulate from the meta-d' model across separate conditions

Generate a simulated dataset across separate conditions from the meta-d'
model with sensitivity `dprime`, response bias `c`, metacognitive
efficiency `log_M`, and distances between confidence thresholds
`c2_0_diff` and `c2_1_diff` (for the two responses).

## Usage

``` r
sim_metad_condition(
  N_trials = 100,
  dprime = rep(1, 2),
  c = rep(0, 2),
  log_M = rep(0, 2),
  c2_0_diff = list(rep(0.5, 3), rep(0.5, 3)),
  c2_1_diff = list(rep(0.5, 3), rep(0.5, 3)),
  metac_absolute = TRUE,
  summarize = FALSE,
  lcdf = normal_lcdf,
  lccdf = normal_lccdf
)
```

## Arguments

- N_trials:

  Total number of trials to simulate. Half of these trials will have
  `stimulus=0` and half will have `stimulus=1`.

- dprime:

  The sensitivity of the signal detection agent to simulate

- c:

  The response bias of the signal detection agent to simulate

- log_M:

  The metacognitive efficiency of the agent on the logarithmic scale,
  where `0` indicates optimal metacognitive sensitivity, negative
  numbers indicate metacognitive inefficiency, and positive numbers
  indicate metacognitive hyper-efficiency.

- c2_0_diff, c2_1_diff:

  Distances between confidence thresholds for `"0"` and `"1"` responses,
  such that `meta_c2_0 = meta_c - cumsum(c2_0_diff)` and
  `meta_c2_1 = meta_c + cumsum(c2_1_diff)`.

- metac_absolute:

  Determines how to fix the type 1 threshold for modeling confidence
  ratings. If metac_absolute=TRUE, `meta_c = c`. Otherwise,
  `meta_c = M * c`.

- summarize:

  Aggregate the data? If `summarize=FALSE`, returns a dataset with one
  row per observation. If `summarize=TRUE`, returns an aggregated
  dataset where `n` is the number of observations per response,
  accuracy, and confidence level.

- lcdf, lccdf:

  The log (complement) cumulative distribution function of the
  underlying signal distribution. By default, uses a
  `normal(+/-dprime/2, 1)` distribution.

## Value

A simulated dataset of type 1 responses and confidence ratings, with
columns:

- `trial`: the simulated trial number

- `condition`: the simulated condition number

- `stimulus`: the value of the stimulus on each trial (either `0` or
  `1`)

- `response`: the simulated type 1 response (either `0` or `1`)

- `correct`: whether `stimulus==response` (either `0` or `1`)

- `confidence`: the simulated type 2 response (from `1` to
  `length(c2_0_diff)+1`)

- `dprime:theta_2`: the simulated agent's parameter values

If `summarize=TRUE`, the `trial` column is replaced with an `n` column
indicating the number of simulated type 1/type 2 responses for each
possible value.

## Examples

``` r
sim_metad_condition(N_trials = 10)
#> # A tibble: 20 × 15
#>    condition trial stimulus response correct confidence dprime     c meta_dprime
#>        <int> <int>    <int>    <int>   <int>      <int>  <dbl> <dbl>       <dbl>
#>  1         1     1        0        0       1          2      1     0           1
#>  2         1     2        0        1       0          1      1     0           1
#>  3         1     3        0        1       0          2      1     0           1
#>  4         1     4        0        1       0          3      1     0           1
#>  5         1     5        0        1       0          4      1     0           1
#>  6         1     1        1        0       0          1      1     0           1
#>  7         1     2        1        0       0          2      1     0           1
#>  8         1     3        1        0       0          4      1     0           1
#>  9         1     4        1        1       1          2      1     0           1
#> 10         1     5        1        1       1          3      1     0           1
#> 11         2     1        0        0       1          2      1     0           1
#> 12         2     2        0        0       1          3      1     0           1
#> 13         2     3        0        0       1          4      1     0           1
#> 14         2     4        0        1       0          1      1     0           1
#> 15         2     5        0        1       0          3      1     0           1
#> 16         2     1        1        1       1          1      1     0           1
#> 17         2     2        1        1       1          1      1     0           1
#> 18         2     3        1        1       1          2      1     0           1
#> 19         2     4        1        1       1          2      1     0           1
#> 20         2     5        1        1       1          4      1     0           1
#> # ℹ 6 more variables: M <dbl>, meta_c2_0 <list>, meta_c2_1 <list>, theta <dbl>,
#> #   theta_1 <dbl>, theta_2 <dbl>
sim_metad_condition(N_trials = 10000, summarize = TRUE)
#> # A tibble: 32 × 15
#>    condition stimulus response correct confidence     n dprime     c meta_dprime
#>        <int>    <int>    <int>   <int>      <int> <int>  <dbl> <dbl>       <dbl>
#>  1         1        0        0       1          1   986      1     0           1
#>  2         1        0        0       1          2   968      1     0           1
#>  3         1        0        0       1          3   758      1     0           1
#>  4         1        0        0       1          4   759      1     0           1
#>  5         1        0        1       0          1   725      1     0           1
#>  6         1        0        1       0          2   474      1     0           1
#>  7         1        0        1       0          3   221      1     0           1
#>  8         1        0        1       0          4   109      1     0           1
#>  9         1        1        0       0          1   779      1     0           1
#> 10         1        1        0       0          2   442      1     0           1
#> # ℹ 22 more rows
#> # ℹ 6 more variables: M <dbl>, meta_c2_0 <list>, meta_c2_1 <list>, theta <dbl>,
#> #   theta_1 <dbl>, theta_2 <dbl>
sim_metad_condition(N_trials = 10, c2_0_diff = list(1, .5), c2_1_diff = list(1, .5))
#> # A tibble: 20 × 15
#>    condition trial stimulus response correct confidence dprime     c meta_dprime
#>        <int> <int>    <int>    <int>   <int>      <int>  <dbl> <dbl>       <dbl>
#>  1         1     1        0        0       1          1      1     0           1
#>  2         1     2        0        0       1          1      1     0           1
#>  3         1     3        0        0       1          1      1     0           1
#>  4         1     4        0        0       1          2      1     0           1
#>  5         1     5        0        0       1          2      1     0           1
#>  6         1     1        1        0       0          1      1     0           1
#>  7         1     2        1        0       0          2      1     0           1
#>  8         1     3        1        0       0          2      1     0           1
#>  9         1     4        1        1       1          1      1     0           1
#> 10         1     5        1        1       1          2      1     0           1
#> 11         2     1        0        0       1          1      1     0           1
#> 12         2     2        0        0       1          2      1     0           1
#> 13         2     3        0        1       0          1      1     0           1
#> 14         2     4        0        1       0          1      1     0           1
#> 15         2     5        0        1       0          2      1     0           1
#> 16         2     1        1        0       0          1      1     0           1
#> 17         2     2        1        0       0          1      1     0           1
#> 18         2     3        1        0       0          2      1     0           1
#> 19         2     4        1        1       1          2      1     0           1
#> 20         2     5        1        1       1          2      1     0           1
#> # ℹ 6 more variables: M <dbl>, meta_c2_0 <list>, meta_c2_1 <list>, theta <dbl>,
#> #   theta_1 <dbl>, theta_2 <dbl>
```
