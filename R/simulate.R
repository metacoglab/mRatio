#' Generate a covariance matrix.
#' @param S a vector of standard deviations
#' @param OMEGA a correlation matrix
#' @returns a `[N x N]` covariance matrix, where `N = length(S)`.
#' @export
cov_matrix <- function(S, OMEGA) {
  diag(S) %*% OMEGA %*% diag(S)
}

#' Generate an `[nrow x nrow]` correlation matrix
#' with all off-diagonal values equal to r
#' @param r the correlation to fill in the matrix off-diagonals
#' @param nrow the number of rows (and columns) of the resulting matrix
#' @returns An `[nrow x nrow]` matrix with values along the diagonal equal
#' to `1` and values off of the diagonal equal to `r`
#' @export
corr_matrix <- function(r, nrow = 2) {
  diag(1 - r, nrow) + r
}

#' Sample from a matrix-normal distribution
#' @param mu a matrix of means
#' @param L_sigma_1 the Cholesky-decomposed covariance matrix for the rows
#' @param L_sigma_2 the Cholesky-decomposed covariance matrix for the columns
#' @returns A single sample from a matrix-normal distribution with mean
#' `mu` (a matrix), row-wise covariances `sigma_1`, and column-wise
#' covariances `sigma_2`, where `L_sigma_1` and `L_sigma_2` are the
#' Cholesky-decomposed covariance matrices
#' @export
rmatrixnorm <- function(mu, L_sigma_1, L_sigma_2) {
  mu +
    L_sigma_1 %*%
    matrix(mvtnorm::rmvnorm(1, mean = rep(0, nrow(L_sigma_1) * nrow(L_sigma_2))),
      nrow = nrow(L_sigma_1)
    ) %*%
    L_sigma_2
}

#' Simulate from the meta-d' model with sensitivity `d_prime`,
#' response bias `c`, metacognitive efficiency `log_M`, and
#' distances between confidence thresholds `c2_0_diff` and `c2_1_diff`
#' (for the two responses).
#' @param N_trials Total number of trials to simulate. Half of these trials
#' will have `stimulus=0` and half will have `stimulus=1`.
#' @param d_prime The sensitivity of the signal detection agent to simulate
#' @param c The response bias of the signal detection agent to simulate
#' @param log_M The metacognitive efficiency of the agent on the logarithmic
#' scale, where `0` indicates optimal metacognitive sensitivity, negative numbers
#' indicate metacognitive inefficiency, and positive numbers indicate
#' metacognitive hyper-efficiency.
#' @param c2_0_diff Distances between confidence thresholds for `"0"` responses,
#' such that `meta_c2_0 = meta_c - cumsum(c2_0_diff)`.
#' @param c2_1_diff Distances between confidence thresholds for `"1"` responses,
#' such that `meta_c2_1 = meta_c + cumsum(c2_1_diff)`.
#' @param metac_absolute Determines how to fix the type 1 threshold for modeling
#' confidence ratings. If metac_absolute=TRUE, `meta_c = c`.
#' Otherwise, `meta_c = M * c`.
#' @param summarize Aggregate the data?
#' If `summarize=FALSE`, returns a dataset with one row per observation.
#' If `summarize=TRUE`, returns an aggregated dataset where `n` is the
#' number of observations per response, accuracy, and confidence level.
#' @param lcdf The log cumulative distribution function of the underlying
#' signal distribution. By default, uses a `normal(+/- d_prime/2, 1)` distribution.
#' @param lccdf The log complement cumulative distribution function of the underlying
#' signal distribution. By default, uses a `normal(+/- d_prime/2, 1)` distribution.
#' @returns A simulated dataset of type 1 responses and confidence ratings,
#' with columns:
#'   - `trial`: the simulated trial number
#'   - `stimulus`: the value of the stimulus on each trial (either `0` or `1`)
#'   - `response`: the simulated type 1 response (either `0` or `1`)
#'   - `correct`: whether `stimulus==response` (either `0` or `1`)
#'   - `confidence`: the simulated type 2 response (from `1` to `length(c2_0_diff)+1`)
#'   - `d_prime`:`theta_2`: the simulated agent's parameter values
#' If `summarize=TRUE`, the `trial` column is replaced with an `n` column indicating
#' the number of simulated type 1/type 2 responses for each possible value.
#' @export
sim_metad <- function(N_trials = 100, d_prime = 1, c = 0, log_M = 0,
                      c2_0_diff = rep(.5, 3), c2_1_diff = rep(.5, 3),
                      metac_absolute = TRUE, summarize = FALSE,
                      lcdf = normal_lcdf, lccdf = normal_lccdf) {
  if (N_trials <= 0) {
    stop("Error: `N_trials` must be greater than 0.")
  }
  if (!all(
    length(d_prime) == 1, length(c) == 1, length(log_M) == 1,
    is.numeric(d_prime), is.numeric(c), is.numeric(log_M)
  )) {
    stop("Error: `d_prime`, `c`, and `log_M` must be single numbers.")
  }
  if (!is.numeric(c2_0_diff) || !is.numeric(c2_1_diff) ||
    length(c2_0_diff) != length(c2_1_diff) ||
    !all(c2_0_diff > 0) || !all(c2_1_diff > 0)) {
    stop("Error: c2_0_diff and c2_1_diff must be positive vectors of the same length")
  }

  M <- exp(log_M)
  meta_d_prime <- M * d_prime
  meta_c <- NULL
  if (metac_absolute) {
    meta_c <- c
  } else {
    meta_c <- M * c
  }
  meta_c2_0 <- meta_c - cumsum(c2_0_diff)
  meta_c2_1 <- meta_c + cumsum(c2_1_diff)

  d <- tidyr::expand_grid(stimulus = 0:1, response = 0:1, confidence = 1:(length(meta_c2_0) + 1)) |>
    mutate(
      correct = as.integer(.data$stimulus == .data$response),
      joint_response = joint_response(.data$response, .data$confidence, length(meta_c2_0) + 1)
    ) |>
    arrange(.data$stimulus, .data$joint_response) |>
    group_by(.data$stimulus) |>
    mutate(
      theta = metad_pmf(first(.data$stimulus), d_prime, c,
        meta_d_prime, meta_c, meta_c2_0, meta_c2_1,
        lcdf = lcdf, lccdf = lccdf
      ),
      theta_1 = ifelse(.data$response, lccdf(c, to_signed(.data$stimulus) * d_prime / 2),
        lcdf(c, to_signed(.data$stimulus) * d_prime / 2)
      ),
      # theta_1=sdt_type1_pmf(first(stimulus), response=response, d_prime, c),
      theta_2 = .data$theta / .data$theta_1,
      n = as.vector(rmultinom(1, N_trials / 2, .data$theta))
    ) |>
    mutate(
      d_prime = d_prime, c = c, meta_d_prime = meta_d_prime,
      M = M, meta_c2_0 = list(meta_c2_0), meta_c2_1 = list(meta_c2_1)
    ) |>
    select(
      "stimulus", "response", "correct", "confidence", "n", "d_prime", "c",
      "meta_d_prime", "M", "meta_c2_0", "meta_c2_1", "theta", "theta_1", "theta_2"
    ) |>
    arrange(.data$stimulus, .data$response, .data$confidence)

  if (summarize) {
    d
  } else {
    d |>
      tidyr::uncount(.data$n) |>
      mutate(trial = row_number()) |>
      relocate(.data$trial)
  }
}

#' Simulate from the meta-d' model across separate conditions with
#' sensitivity `d_prime`, response bias `c`, metacognitive efficiency `log_M`, and
#' distances between confidence thresholds `c2_0_diff` and `c2_1_diff`
#' (for the two responses).
#' @param N_trials Total number of trials to simulate. Half of these trials
#' will have `stimulus=0` and half will have `stimulus=1`.
#' @param d_prime The sensitivity of the signal detection agent to simulate
#' @param c The response bias of the signal detection agent to simulate
#' @param log_M The metacognitive efficiency of the agent on the logarithmic
#' scale, where `0` indicates optimal metacognitive sensitivity, negative numbers
#' indicate metacognitive inefficiency, and positive numbers indicate
#' metacognitive hyper-efficiency.
#' @param c2_0_diff Distances between confidence thresholds for `"0"` responses,
#' such that `meta_c2_0 = meta_c - cumsum(c2_0_diff)`.
#' @param c2_1_diff Distances between confidence thresholds for `"1"` responses,
#' such that `meta_c2_1 = meta_c + cumsum(c2_1_diff)`.
#' @param metac_absolute Determines how to fix the type 1 threshold for modeling
#' confidence ratings. If metac_absolute=TRUE, `meta_c = c`.
#' Otherwise, `meta_c = M * c`.
#' @param summarize Aggregate the data?
#' If `summarize=FALSE`, returns a dataset with one row per observation.
#' If `summarize=TRUE`, returns an aggregated dataset where `n` is the
#' number of observations per response, accuracy, and confidence level.
#' @param lcdf The log cumulative distribution function of the underlying
#' signal distribution. By default, uses a `normal(+/- d_prime/2, 1)` distribution.
#' @param lccdf The log complement cumulative distribution function of the underlying
#' signal distribution. By default, uses a `normal(+/- d_prime/2, 1)` distribution.
#' @returns A simulated dataset of type 1 responses and confidence ratings,
#' with columns:
#'   - `trial`: the simulated trial number
#'   - `stimulus`: the value of the stimulus on each trial (either `0` or `1`)
#'   - `response`: the simulated type 1 response (either `0` or `1`)
#'   - `correct`: whether `stimulus==response` (either `0` or `1`)
#'   - `confidence`: the simulated type 2 response (from `1` to `length(c2_0_diff)+1`)
#'   - `d_prime`:`theta_2`: the simulated agent's parameter values
#' If `summarize=TRUE`, the `trial` column is replaced with an `n` column indicating
#' the number of simulated type 1/type 2 responses for each possible value.
#' @export
sim_metad_condition <- function(N_trials = 100, d_prime = rep(1, 2), c = rep(0, 2), log_M = rep(0, 2),
                                c2_0_diff = list(rep(.5, 3), rep(.5, 3)),
                                c2_1_diff = list(rep(.5, 3), rep(.5, 3)),
                                metac_absolute = TRUE, summarize = FALSE,
                                lcdf = normal_lcdf, lccdf = normal_lccdf) {
  check_installed("purrr", reason = "to use `purrr::pmap`")

  tibble(
    condition = seq_along(d_prime),
    d_prime = d_prime, c = c, log_M = log_M,
    c2_0_diff = c2_0_diff, c2_1_diff = c2_1_diff
  ) |>
    mutate(data = purrr::pmap(
      list(
        .data$d_prime, .data$c, .data$log_M,
        .data$c2_0_diff, .data$c2_1_diff
      ),
      sim_metad,
      N = N_trials, summarize = summarize,
      metac_absolute = metac_absolute,
      lcdf = lcdf, lccdf = lccdf
    )) |>
    select("condition", "data") |>
    tidyr::unnest(.data$data)
}


#' Simulate from the hierarchical meta-d' model over participants with
#' sensitivity `d_prime`, response bias `c`, metacognitive efficiency `log_M`, and
#' distances between confidence thresholds `c2_0_diff` and `c2_1_diff`
#' (for the two responses).
#' @param N_trials Total number of trials to simulate per participant.
#' Half of these trials will have `stimulus=0` and half will have `stimulus=1`.
#' @param N_participants Total number of participants to simulate
#' @param mu_d_prime The mean sensitivity of the signal detection agents to simulate
#' @param sd_d_prime The standard deviation of sensitivities of the signal detection agents to simulate
#' @param mu_c The mean response bias of the signal detection agents to simulate
#' @param sd_c The standard deviation of response biases of the signal detection agents to simulate
#' @param mu_log_M The mean metacognitive efficiency of the agents on the logarithmic
#' scale, where `0` indicates optimal metacognitive sensitivity, negative numbers
#' indicate metacognitive inefficiency, and positive numbers indicate
#' metacognitive hyper-efficiency.
#' @param sd_log_M The standard deviation of metacognitive efficiency of the agents
#' on the logarithmic scale.
#' @param mu_z_c2_0 Mean distance between confidence thresholds for `"0"`
#' responses on the log_scale, such that `meta_c2_0 = meta_c - cumulative_sum(exp(z_c2_0))`.
#' @param sd_z_c2_0 SD of log distances between confidence thresholds for `"0"`
#' responses on the log_scale.
#' @param r_z_c2_0 Correlation of log distances between confidence thresholds
#' for `"0"` responses on the log_scale.
#' @param mu_z_c2_1 Mean distance between confidence thresholds for `"1"` responses,
#' such that `meta_c2_1 = meta_c + cumulative_sum(exp(z_c2_1))`.
#' @param sd_z_c2_1 SD of log distances between confidence thresholds for `"1"`
#' responses on the log_scale.
#' @param r_z_c2_1 Correlation of log distances between confidence thresholds
#' for `"1"` responses on the log_scale.
#' @param metac_absolute Determines how to fix the type 1 threshold for modeling
#' confidence ratings. If metac_absolute=TRUE, `meta_c = c`.
#' Otherwise, `meta_c = M * c`.
#' @param summarize Aggregate the data?
#' If summarize=FALSE, returns a dataset with one row per observation.
#' If summarize=TRUE, returns an aggregated dataset where `n` is the
#' number of observations per response, accuracy, and confidence level.
#' @param lcdf The log cumulative distribution function of the underlying
#' signal distribution. By default, uses a `normal(+/- d_prime/2, 1)` distribution.
#' @param lccdf The log complement cumulative distribution function of the underlying
#' signal distribution. By default, uses a `normal(+/- d_prime/2, 1)` distribution.
#' @returns A simulated dataset of type 1 responses and confidence ratings,
#' with columns:
#'   - `trial`: the simulated trial number
#'   - `stimulus`: the value of the stimulus on each trial (either `0` or `1`)
#'   - `response`: the simulated type 1 response (either `0` or `1`)
#'   - `correct`: whether `stimulus==response` (either `0` or `1`)
#'   - `confidence`: the simulated type 2 response (from `1` to `length(c2_0_diff)+1`)
#'   - `d_prime`:`theta_2`: the simulated agent's parameter values
#' If `summarize=TRUE`, the `trial` column is replaced with an `n` column indicating
#' the number of simulated type 1/type 2 responses for each possible value.
#' @export
sim_metad_participant <- function(N_participants = 100, N_trials = 100,
                                  mu_d_prime = 1, sd_d_prime = .5, mu_c = 0, sd_c = .5,
                                  mu_log_M = 0, sd_log_M = .5,
                                  mu_z_c2_0 = rep(-1, 3), sd_z_c2_0 = rep(.1, 3), r_z_c2_0 = diag(3),
                                  mu_z_c2_1 = rep(-1, 3), sd_z_c2_1 = rep(.1, 3), r_z_c2_1 = diag(3),
                                  metac_absolute = TRUE, summarize = FALSE,
                                  lcdf = normal_lcdf, lccdf = normal_lccdf) {
  check_installed("purrr", reason = "to use `purrr::map` and `purrr::pmap`")

  tidyr::expand_grid(participant = 1:N_participants) |>
    mutate(
      d_prime = rnorm(n(), mu_d_prime, sd_d_prime),
      c = rnorm(n(), mu_c, sd_c),
      log_M = rnorm(n(), mu_log_M, sd_log_M),
      c2_0_diff = purrr::map(
        .data$participant,
        ~ exp(rmvnorm(1,
          mean = mu_z_c2_0,
          sigma = cov_matrix(sd_z_c2_0, r_z_c2_0)
        ))
      ),
      c2_1_diff = purrr::map(
        .data$participant,
        ~ exp(rmvnorm(1,
          mean = mu_z_c2_1,
          sigma = cov_matrix(sd_z_c2_1, r_z_c2_1)
        ))
      ),
      data = purrr::pmap(
        list(
          .data$d_prime, .data$c, .data$log_M,
          .data$c2_0_diff, .data$c2_1_diff
        ),
        sim_metad,
        N = N_trials, metac_absolute = metac_absolute,
        summarize = summarize, lcdf = lcdf, lccdf = lccdf
      )
    ) |>
    select("participant", "data") |>
    tidyr::unnest(.data$data)
}

#' Simulate from the hierarchical meta-d' model over participants across separate
#' within-participant conditions with sensitivity `d_prime`, response bias `c`,
#' metacognitive efficiency `log_M`, and distances between confidence thresholds
#' `c2_0_diff` and `c2_1_diff` (for the two responses).
#' @param N_trials Total number of trials to simulate per participant.
#' Half of these trials will have `stimulus=0` and half will have `stimulus=1`.
#' @param N_participants Total number of participants to simulate.
#' @param mu_d_prime The mean sensitivity of the signal detection agents to simulate
#' @param sd_d_prime The standard deviation of sensitivities of the signal detection agents to simulate
#' @param r_d_prime The correlations between participant-level sensitivities across conditions
#' @param mu_c The mean response bias of the signal detection agents to simulate
#' @param sd_c The standard deviation of response biases of the signal detection agents to simulate
#' @param r_c The correlations between participant-level response biases across conditions
#' @param mu_log_M The mean metacognitive efficiency of the agents on the logarithmic
#' scale, where `0` indicates optimal metacognitive sensitivity, negative numbers
#' indicate metacognitive inefficiency, and positive numbers indicate
#' metacognitive hyper-efficiency.
#' @param sd_log_M The standard deviation of metacognitive efficiency of the agents
#' on the logarithmic scale.
#' @param r_log_M The correlations between participant-level metacognitive efficiencies across conditions
#' @param mu_z_c2_0 Mean distance between confidence thresholds for `"0"`
#' responses on the log_scale, such that `meta_c2_0 = meta_c - cumulative_sum(exp(z_c2_0))`.
#' @param sd_z_c2_0_condition SD of log distances across conditions between
#' confidence thresholds for `"0"` responses on the log_scale.
#' @param sd_z_c2_0_confidence SD of log distances across confidence levels
#' between confidence thresholds for `"0"` responses on the log_scale.
#' @param r_z_c2_0_condition Correlation across conditions of log distances
#' between confidence thresholds for `"0"` responses on the log_scale.
#' @param r_z_c2_0_confidence Correlation across confidence levels of log distances
#' between confidence thresholds for `"0"` responses on the log_scale.
#' @param mu_z_c2_1 Mean distance between confidence thresholds for `"1"` responses,
#' such that `meta_c2_1 = meta_c + cumulative_sum(exp(z_c2_1))`.
#' @param sd_z_c2_1_condition SD of log distances across conditions between
#' confidence thresholds for `"1"` responses on the log_scale.
#' @param sd_z_c2_1_confidence SD of log distances across confidence levels
#' between confidence thresholds for `"1"` responses on the log_scale.
#' @param r_z_c2_1_condition Correlation across conditions of log distances
#' between confidence thresholds for `"1"` responses on the log_scale.
#' @param r_z_c2_1_confidence Correlation across confidence levels of log distances
#' between confidence thresholds for `"1"` responses on the log_scale.
#' @param metac_absolute Determines how to fix the type 1 threshold for modeling
#' confidence ratings. If metac_absolute=TRUE, `meta_c = c`.
#' Otherwise, `meta_c = M * c`.
#' @param summarize Aggregate the data?
#' If summarize=FALSE, returns a dataset with one row per observation.
#' If summarize=TRUE, returns an aggregated dataset where `n` is the
#' number of observations per response, accuracy, and confidence level.
#' @param lcdf The log cumulative distribution function of the underlying
#' signal distribution. By default, uses a `normal(+/- d_prime/2, 1)` distribution.
#' @param lccdf The log complement cumulative distribution function of the underlying
#' signal distribution. By default, uses a `normal(+/- d_prime/2, 1)` distribution.
#' @returns A simulated dataset of type 1 responses and confidence ratings,
#' with columns:
#'   - `trial`: the simulated trial number
#'   - `stimulus`: the value of the stimulus on each trial (either `0` or `1`)
#'   - `response`: the simulated type 1 response (either `0` or `1`)
#'   - `correct`: whether `stimulus==response` (either `0` or `1`)
#'   - `confidence`: the simulated type 2 response (from `1` to `length(c2_0_diff)+1`)
#'   - `d_prime`:`theta_2`: the simulated agent's parameter values
#' If `summarize=TRUE`, the `trial` column is replaced with an `n` column indicating
#' the number of simulated type 1/type 2 responses for each possible value.
#' @export
sim_metad_participant_condition <-
  function(N_participants = 100, N_trials = 100,
           mu_d_prime = rep(1, 2), sd_d_prime = rep(.5, 2), r_d_prime = diag(2),
           mu_c = rep(0, 2), sd_c = rep(.5, 2), r_c = diag(2),
           mu_log_M = rep(0, 2), sd_log_M = rep(.5, 2), r_log_M = diag(2),
           mu_z_c2_0 = matrix(rep(-1, 6), nrow = 3, ncol = 2),
           sd_z_c2_0_condition = rep(.1, 2), r_z_c2_0_condition = diag(2),
           sd_z_c2_0_confidence = rep(.1, 3), r_z_c2_0_confidence = diag(3),
           mu_z_c2_1 = matrix(rep(-1, 6), nrow = 3, ncol = 2),
           sd_z_c2_1_condition = rep(.1, 2), r_z_c2_1_condition = diag(2),
           sd_z_c2_1_confidence = rep(.1, 3), r_z_c2_1_confidence = diag(3),
           metac_absolute = TRUE, summarize = FALSE,
           lcdf = normal_lcdf, lccdf = normal_lccdf) {
    check_installed("purrr", reason = "to use `purrr::map_dbl`, `purrr::map` and `purrr::pmap`")

    ## calculate covariance matrices
    sigma_d_prime <- cov_matrix(sd_d_prime, r_d_prime)
    sigma_c <- cov_matrix(sd_c, r_c)
    sigma_log_M <- cov_matrix(sd_log_M, r_log_M)
    L_sigma_z_c2_0_condition <- chol(cov_matrix(sd_z_c2_0_condition, r_z_c2_0_condition))
    L_sigma_z_c2_0_confidence <- chol(cov_matrix(sd_z_c2_0_confidence, r_z_c2_0_confidence))
    L_sigma_z_c2_1_condition <- chol(cov_matrix(sd_z_c2_1_condition, r_z_c2_1_condition))
    L_sigma_z_c2_1_confidence <- chol(cov_matrix(sd_z_c2_1_confidence, r_z_c2_1_confidence))

    tidyr::expand_grid(
      participant = 1:N_participants,
      condition = seq_along(mu_d_prime)
    ) |>
      group_by(.data$participant) |>
      mutate(
        d_prime = purrr::map_dbl(
          .data$condition, function(condition, d) d[, condition],
          mvtnorm::rmvnorm(1, mu_d_prime, sigma_d_prime)
        ),
        c = purrr::map_dbl(
          .data$condition, function(condition, c) c[, condition],
          mvtnorm::rmvnorm(1, mu_c, sigma_c)
        ),
        log_M = purrr::map_dbl(
          .data$condition, function(condition, log_m) log_m[, condition],
          mvtnorm::rmvnorm(1, mu_log_M, sigma_log_M)
        ),
        c2_0_diff = purrr::map(
          .data$condition, function(condition, c2) c2[, condition],
          exp(rmatrixnorm(
            mu_z_c2_0,
            L_sigma_z_c2_0_confidence,
            L_sigma_z_c2_0_condition
          ))
        ),
        c2_1_diff = purrr::map(
          .data$condition, function(condition, c2) c2[, condition],
          exp(rmatrixnorm(
            mu_z_c2_1,
            L_sigma_z_c2_1_confidence,
            L_sigma_z_c2_1_condition
          ))
        )
      ) |>
      ungroup() |>
      mutate(data = purrr::pmap(
        list(
          .data$d_prime, .data$c, .data$log_M,
          .data$c2_0_diff, .data$c2_1_diff
        ),
        sim_metad,
        N = N_trials, metac_absolute = metac_absolute,
        summarize = summarize, lcdf = lcdf, lccdf = lccdf
      )) |>
      select("participant", "condition", "data") |>
      tidyr::unnest(.data$data)
  }
