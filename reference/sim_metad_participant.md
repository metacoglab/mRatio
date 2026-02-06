# Simulate from the hierarchical meta-d' model

Generate a simulated dataset across participants from the meta-d' model
with sensitivity `dprime`, response bias `c`, metacognitive efficiency
`log_M`, and distances between confidence thresholds `c2_0_diff` and
`c2_1_diff` (for the two responses).

## Usage

``` r
sim_metad_participant(
  N_participants = 100,
  N_trials = 100,
  mu_dprime = 1,
  sd_dprime = 0.5,
  mu_c = 0,
  sd_c = 0.5,
  mu_log_M = 0,
  sd_log_M = 0.5,
  mu_z_c2_0 = rep(-1, 3),
  sd_z_c2_0 = rep(0.1, 3),
  r_z_c2_0 = diag(3),
  mu_z_c2_1 = rep(-1, 3),
  sd_z_c2_1 = rep(0.1, 3),
  r_z_c2_1 = diag(3),
  metac_absolute = TRUE,
  summarize = FALSE,
  lcdf = normal_lcdf,
  lccdf = normal_lccdf
)
```

## Arguments

- N_trials, N_participants:

  Total number of participants and trials to simulate per participant.
  Half of these trials will have `stimulus=0` and half will have
  `stimulus=1`.

- mu_dprime, sd_dprime:

  The mean and standard deviation of sensitivities of the signal
  detection agents to simulate

- mu_c, sd_c:

  The mean and standard deviation of response bias of the signal
  detection agents to simulate

- mu_log_M, sd_log_M:

  The mean and standard deviation of metacognitive efficiency of the
  agents on the logarithmic scale, where `0` indicates optimal
  metacognitive sensitivity, negative numbers indicate metacognitive
  inefficiency, and positive numbers indicate metacognitive
  hyper-efficiency.

- mu_z_c2_0, mu_z_c2_1:

  Mean distance between confidence thresholds for `"0"` and `"1"`
  responses on the log_scale, such that
  `meta_c2_0 = meta_c - cumulative_sum(exp(z_c2_0))` and
  `meta_c2_1 = meta_c + cumulative_sum(exp(z_c2_1))`.

- sd_z_c2_0, sd_z_c2_1:

  SD of log distances between confidence thresholds for `"0"` and `"1"`
  responses on the log_scale.

- r_z_c2_0, r_z_c2_1:

  Correlation of log distances between confidence thresholds for `"0"`
  and `"1"` responses on the log_scale.

- metac_absolute:

  Determines how to fix the type 1 threshold for modeling confidence
  ratings. If metac_absolute=TRUE, `meta_c = c`. Otherwise,
  `meta_c = M * c`.

- summarize:

  Aggregate the data? If summarize=FALSE, returns a dataset with one row
  per observation. If summarize=TRUE, returns an aggregated dataset
  where `n` is the number of observations per response, accuracy, and
  confidence level.

- lcdf:

  The log cumulative distribution function of the underlying signal
  distribution. By default, uses a `normal(+/-dprime/2, 1)`
  distribution.

- lccdf:

  The log complement cumulative distribution function of the underlying
  signal distribution. By default, uses a `normal(+/-dprime/2, 1)`
  distribution.

## Value

A simulated dataset of type 1 responses and confidence ratings, with
columns:

- `trial`: the simulated trial number

- `participant`: the simulated participant number

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
sim_metad_participant(N_participants = 10, N_trials = 10)
#> # A tibble: 100 × 15
#>    participant trial stimulus response correct confidence dprime       c
#>          <int> <int>    <int>    <int>   <int>      <int>  <dbl>   <dbl>
#>  1           1     1        0        0       1          1 -0.381 -0.0327
#>  2           1     2        0        0       1          3 -0.381 -0.0327
#>  3           1     3        0        1       0          3 -0.381 -0.0327
#>  4           1     4        0        1       0          3 -0.381 -0.0327
#>  5           1     5        0        1       0          3 -0.381 -0.0327
#>  6           1     1        1        0       0          4 -0.381 -0.0327
#>  7           1     2        1        1       1          2 -0.381 -0.0327
#>  8           1     3        1        1       1          3 -0.381 -0.0327
#>  9           1     4        1        1       1          4 -0.381 -0.0327
#> 10           1     5        1        1       1          4 -0.381 -0.0327
#> # ℹ 90 more rows
#> # ℹ 7 more variables: meta_dprime <dbl>, M <dbl>, meta_c2_0 <list>,
#> #   meta_c2_1 <list>, theta <dbl>, theta_1 <dbl>, theta_2 <dbl>
sim_metad_participant(mu_dprime = 2, mu_log_M = -1)
#> # A tibble: 10,000 × 15
#>    participant trial stimulus response correct confidence dprime      c
#>          <int> <int>    <int>    <int>   <int>      <int>  <dbl>  <dbl>
#>  1           1     1        0        0       1          1   2.19 -0.175
#>  2           1     2        0        0       1          1   2.19 -0.175
#>  3           1     3        0        0       1          1   2.19 -0.175
#>  4           1     4        0        0       1          1   2.19 -0.175
#>  5           1     5        0        0       1          1   2.19 -0.175
#>  6           1     6        0        0       1          1   2.19 -0.175
#>  7           1     7        0        0       1          1   2.19 -0.175
#>  8           1     8        0        0       1          1   2.19 -0.175
#>  9           1     9        0        0       1          1   2.19 -0.175
#> 10           1    10        0        0       1          1   2.19 -0.175
#> # ℹ 9,990 more rows
#> # ℹ 7 more variables: meta_dprime <dbl>, M <dbl>, meta_c2_0 <list>,
#> #   meta_c2_1 <list>, theta <dbl>, theta_1 <dbl>, theta_2 <dbl>
```
