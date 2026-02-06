# Package index

## All functions

- [`aggregate_metad()`](https://metacoglab.github.io/mRatio/reference/aggregate_metad.md)
  :

  Aggregate `data` by `response`, `confidence`, and other columns

- [`metacognitive_bias_draws()`](https://metacoglab.github.io/mRatio/reference/bias_draws.md)
  [`add_metacognitive_bias_draws()`](https://metacoglab.github.io/mRatio/reference/bias_draws.md)
  : Obtain posterior draws of an index of metacognitive bias

- [`corr_matrix()`](https://metacoglab.github.io/mRatio/reference/corr_matrix.md)
  :

  Generate a correlation matrix with all off-diagonal values equal to
  `r`

- [`cov_matrix()`](https://metacoglab.github.io/mRatio/reference/cov_matrix.md)
  : Generate a covariance matrix.

- [`fit_metad()`](https://metacoglab.github.io/mRatio/reference/fit_metad.md)
  :

  Fit the meta-d' model using `brms` package

- [`get_dist()`](https://metacoglab.github.io/mRatio/reference/get_dist.md)
  : Get the R function for the model's underlying distribution functions

- [`get_metac()`](https://metacoglab.github.io/mRatio/reference/get_metac.md)
  :

  Get the parameterization of `meta_c` in `model`

- [`log_lik_metad()`](https://metacoglab.github.io/mRatio/reference/log_lik_metad.md)
  : Generate a function to calculate the log likelihood of the metad'
  model

- [`lp_metad()`](https://metacoglab.github.io/mRatio/reference/lp_metad.md)
  : Calculate the log probability simplex of the metad' model

- [`mean_confidence_draws()`](https://metacoglab.github.io/mRatio/reference/mean_conf_draws.md)
  [`add_mean_confidence_draws()`](https://metacoglab.github.io/mRatio/reference/mean_conf_draws.md)
  : Obtain posterior draws of mean confidence

- [`metacognitive_bias()`](https://metacoglab.github.io/mRatio/reference/metacognitive_bias.md)
  : Given the distances between successive confidence thresholds,
  calculate the average of the cumulative distances to 0.

- [`metad()`](https://metacoglab.github.io/mRatio/reference/metad.md) :

  `brms` family for the metad' model

- [`metad_pmf()`](https://metacoglab.github.io/mRatio/reference/metad_pmf.md)
  : Generate (log) probability simplex over the joint type 1/type 2
  responses

- [`normal_lcdf()`](https://metacoglab.github.io/mRatio/reference/normal_dist.md)
  [`normal_lccdf()`](https://metacoglab.github.io/mRatio/reference/normal_dist.md)
  : Normal cumulative distribution functions

- [`posterior_epred_metad()`](https://metacoglab.github.io/mRatio/reference/posterior_epred_metad.md)
  : Generate posterior predictions for the metad' model

- [`posterior_predict_metad()`](https://metacoglab.github.io/mRatio/reference/posterior_predict_metad.md)
  : Simulate posterior predictions from the metad' model

- [`response_probabilities()`](https://metacoglab.github.io/mRatio/reference/response_probabilities.md)
  : Compute joint response probabilities from aggregated counts

- [`joint_response()`](https://metacoglab.github.io/mRatio/reference/responses.md)
  [`type1_response()`](https://metacoglab.github.io/mRatio/reference/responses.md)
  [`type2_response()`](https://metacoglab.github.io/mRatio/reference/responses.md)
  : Convert between separate and joint type 1/type 2 responses

- [`rmatrixnorm()`](https://metacoglab.github.io/mRatio/reference/rmatrixnorm.md)
  : Sample from a matrix-normal distribution

- [`roc1_draws()`](https://metacoglab.github.io/mRatio/reference/roc1_draws.md)
  [`add_roc1_draws()`](https://metacoglab.github.io/mRatio/reference/roc1_draws.md)
  : Obtain posterior draws of the pseudo type 1 receiver operating
  characteristic (ROC) curve.

- [`roc2_draws()`](https://metacoglab.github.io/mRatio/reference/roc2_draws.md)
  [`add_roc2_draws()`](https://metacoglab.github.io/mRatio/reference/roc2_draws.md)
  : Obtain posterior draws of the response-specific type 2 receiver
  operating characteristic (ROC) curves.

- [`to_signed()`](https://metacoglab.github.io/mRatio/reference/signed.md)
  [`to_unsigned()`](https://metacoglab.github.io/mRatio/reference/signed.md)
  :

  Convert binary variable `x` between `{0, 1}` and `{-1, 1}`

- [`sim_metad()`](https://metacoglab.github.io/mRatio/reference/sim_metad.md)
  : Simulate from the meta-d' model

- [`sim_metad_condition()`](https://metacoglab.github.io/mRatio/reference/sim_metad_condition.md)
  : Simulate from the meta-d' model across separate conditions

- [`sim_metad_participant()`](https://metacoglab.github.io/mRatio/reference/sim_metad_participant.md)
  : Simulate from the hierarchical meta-d' model

- [`sim_metad_participant_condition()`](https://metacoglab.github.io/mRatio/reference/sim_metad_participant_condition.md)
  : Simulate from the hierarchical meta-d' model across
  within-participant conditions

- [`stancode_metad()`](https://metacoglab.github.io/mRatio/reference/stancode_metad.md)
  : Generate Stan code for the meta-d' model
