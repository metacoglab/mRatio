# Simulate from the hierarchical meta-d' model across within-participant conditions

Generate a simulated dataset across participants and conditions from the
meta-d' model with sensitivity `dprime`, response bias `c`,
metacognitive efficiency `log_M`, and distances between confidence
thresholds `c2_0_diff` and `c2_1_diff` (for the two responses).

## Usage

``` r
sim_metad_participant_condition(
  N_participants = 100,
  N_trials = 100,
  mu_dprime = rep(1, 2),
  sd_dprime = rep(0.5, 2),
  r_dprime = diag(2),
  mu_c = rep(0, 2),
  sd_c = rep(0.5, 2),
  r_c = diag(2),
  mu_log_M = rep(0, 2),
  sd_log_M = rep(0.5, 2),
  r_log_M = diag(2),
  mu_z_c2_0 = matrix(rep(-1, 6), nrow = 3, ncol = 2),
  sd_z_c2_0_condition = rep(0.1, 2),
  r_z_c2_0_condition = diag(2),
  sd_z_c2_0_confidence = rep(0.1, 3),
  r_z_c2_0_confidence = diag(3),
  mu_z_c2_1 = matrix(rep(-1, 6), nrow = 3, ncol = 2),
  sd_z_c2_1_condition = rep(0.1, 2),
  r_z_c2_1_condition = diag(2),
  sd_z_c2_1_confidence = rep(0.1, 3),
  r_z_c2_1_confidence = diag(3),
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

- mu_dprime, sd_dprime, r_dprime:

  The mean, standard deviation, and within-participant correlations of
  sensitivities of the signal detection agents to simulate

- mu_c, sd_c, r_c:

  The mean, standard deviation, and within-participant correlations of
  response bias of the signal detection agents to simulate

- mu_log_M, sd_log_M, r_log_M:

  The mean, standard deviation, and within-participant correlations of
  metacognitive efficiency of the agents on the logarithmic scale, where
  `0` indicates optimal metacognitive sensitivity, negative numbers
  indicate metacognitive inefficiency, and positive numbers indicate
  metacognitive hyper-efficiency.

- mu_z_c2_0, mu_z_c2_1:

  Mean distance between confidence thresholds for `"0"` and `"1"`
  responses on the log_scale, such that
  `meta_c2_0 = meta_c - cumulative_sum(exp(z_c2_0))` and
  `meta_c2_1 = meta_c + cumulative_sum(exp(z_c2_1))`.

- sd_z_c2_0_condition, sd_z_c2_1_condition:

  SD of log distances across conditions between confidence thresholds
  for `"0"` and `"1"` responses on the log_scale.

- r_z_c2_0_condition, r_z_c2_1_condition:

  Correlation across conditions of log distances between confidence
  thresholds for `"0"` and `"1"` responses on the log_scale.

- sd_z_c2_0_confidence, sd_z_c2_1_confidence:

  SD of log distances across confidence levels between confidence
  thresholds for `"0"` and `"1"` responses on the log_scale.

- r_z_c2_0_confidence, r_z_c2_1_confidence:

  Correlation across confidence levels of log distances between
  confidence thresholds for `"0"` and `"1"` responses on the log_scale.

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
  distribution. By default, uses a `normal(+/- dprime/2, 1)`
  distribution.

- lccdf:

  The log complement cumulative distribution function of the underlying
  signal distribution. By default, uses a `normal(+/- dprime/2, 1)`
  distribution.

## Value

A simulated dataset of type 1 responses and confidence ratings, with
columns:

- `trial`: the simulated trial number

- `stimulus`: the value of the stimulus on each trial (either `0` or
  `1`)

- `response`: the simulated type 1 response (either `0` or `1`)

- `correct`: whether `stimulus==response` (either `0` or `1`)

- `confidence`: the simulated type 2 response (from `1` to
  `length(c2_0_diff)+1`)

- `dprime:theta_2`: the simulated agent's parameter values If
  `summarize=TRUE`, the `trial` column is replaced with an `n` column
  indicating the number of simulated type 1/type 2 responses for each
  possible value.

## Examples

``` r
sim_metad_participant_condition(10, 10)
#> # A tibble: 200 × 16
#>    participant condition trial stimulus response correct confidence dprime
#>          <int>     <int> <int>    <int>    <int>   <int>      <int>  <dbl>
#>  1           1         1     1        0        0       1          2  0.120
#>  2           1         1     2        0        1       0          2  0.120
#>  3           1         1     3        0        1       0          4  0.120
#>  4           1         1     4        0        1       0          4  0.120
#>  5           1         1     5        0        1       0          4  0.120
#>  6           1         1     1        1        0       0          1  0.120
#>  7           1         1     2        1        0       0          1  0.120
#>  8           1         1     3        1        0       0          2  0.120
#>  9           1         1     4        1        1       1          1  0.120
#> 10           1         1     5        1        1       1          2  0.120
#> # ℹ 190 more rows
#> # ℹ 8 more variables: c <dbl>, meta_dprime <dbl>, M <dbl>, meta_c2_0 <list>,
#> #   meta_c2_1 <list>, theta <dbl>, theta_1 <dbl>, theta_2 <dbl>
```
