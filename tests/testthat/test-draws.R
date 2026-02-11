library(tidyr)
library(tidybayes)

d <- sim_metad(
  N_trials = 1000000, dprime = .75, c = -.5, log_M = -1,
  c2_0 = c(.25, .75, 1), c2_1 = c(.5, 1, 1.25)
)
m <- fit_metad(N ~ 1, d)
newdata <- tibble(.row = 1)

test_that("metad_draws works", {
  m |>
    metad_draws(newdata, pivot_longer = TRUE) |>
    median_qi() |>
    nrow() |>
    expect_equal(11)

  m |>
    metad_draws(newdata) |>
    median_qi(M) |>
    nrow() |>
    expect_equal(1)

  expect_equal(
    metad_draws(m, newdata),
    add_metad_draws(newdata, m)
  )

  m |>
    metad_rvars(newdata, pivot_longer = TRUE) |>
    median_qi() |>
    nrow() |>
    expect_equal(11)

  m |>
    metad_rvars(newdata) |>
    nrow() |>
    expect_equal(1)

  expect_equal(
    metad_rvars(m, newdata),
    add_metad_rvars(newdata, m)
  )

  ## compare between _draws and _rvars
  metad_draws(m, newdata, pivot_longer=TRUE) |>
    median_qi() |>
    arrange(.variable) |>
    pull(.value) |>
    near(metad_rvars(m, newdata, pivot_longer=TRUE) |>
           median_qi() |>
           arrange(.variable) |>
           pull(.value),
         tol=.01
    ) |>
    expect_all_true()
})

test_that("mean_confidence_draws works", {
  m |>
    mean_confidence_draws(newdata) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by(stimulus, response) |>
        summarize(m = mean(confidence), .groups='keep') |>
        pull(m),
      tol = .01
    ) |>
    all() |>
    expect_equal(TRUE)

  m |>
    mean_confidence_draws(newdata, by_stimulus = FALSE) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by(response) |>
        summarize(m = mean(confidence), .groups='keep') |>
        pull(m),
      tol = .01
    ) |>
    all() |>
    expect_equal(TRUE)

  m |>
    mean_confidence_draws(newdata, by_response = FALSE) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by(stimulus) |>
        summarize(m = mean(confidence), .groups='keep') |>
        pull(m),
      tol = .01
    ) |>
    all() |>
    expect_equal(TRUE)

  m |>
    mean_confidence_draws(newdata, by_stimulus = FALSE, by_response = FALSE) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by() |>
        summarize(m = mean(confidence), .groups='keep') |>
        pull(m),
      tol = .01
    ) |>
    all() |>
    expect_equal(TRUE)

  expect_equal(
    mean_confidence_draws(m, newdata),
    add_mean_confidence_draws(newdata, m)
  )


  m |>
    mean_confidence_rvars(newdata) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by(stimulus, response) |>
        summarize(m = mean(confidence), .groups='keep') |>
        pull(m),
      tol = .01
    ) |>
    all() |>
    expect_equal(TRUE)

  m |>
    mean_confidence_rvars(newdata, by_stimulus = FALSE) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by(response) |>
        summarize(m = mean(confidence), .groups='keep') |>
        pull(m),
      tol = .01
    ) |>
    all() |>
    expect_equal(TRUE)

  m |>
    mean_confidence_rvars(newdata, by_response = FALSE) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by(stimulus) |>
        summarize(m = mean(confidence), .groups='keep') |>
        pull(m),
      tol = .01
    ) |>
    all() |>
    expect_equal(TRUE)

  m |>
    mean_confidence_rvars(newdata, by_stimulus = FALSE, by_response = FALSE) |>
    median_qi() |>
    pull(.epred) |>
    near(
      d |> group_by() |>
        summarize(m = mean(confidence), .groups='keep') |>
        pull(m),
      tol = .01
    ) |>
    all() |>
    expect_equal(TRUE)

  expect_equal(
    mean_confidence_rvars(m, newdata),
    add_mean_confidence_rvars(newdata, m)
  )
})


test_that("metacognitive_bias_draws works", {
  draws <- metacognitive_bias_draws(m, newdata)

  expect_equal(nrow(median_qi(draws)), 2)

  draws |>
    mutate(test = metacognitive_bias > 0) |>
    pull(test) |>
    all() |>
    expect_equal(TRUE)

  expect_equal(draws, add_metacognitive_bias_draws(newdata, m))


  draws2 <- metacognitive_bias_rvars(m, newdata)

  expect_equal(nrow(median_qi(draws2)), 2)

  draws2 |>
    mutate(test = all(metacognitive_bias > 0)) |>
    pull(test) |>
    all() |>
    expect_equal(TRUE)

  expect_equal(draws2, add_metacognitive_bias_rvars(newdata, m))

  ## compare between _draws and _rvars
  draws |>
    median_qi() |>
    pull(metacognitive_bias) |>
    near(draws2 |>
           median_qi() |>
           pull(metacognitive_bias),
         tol=.01) |>
    expect_all_true()
})


test_that("roc1_draws works", {
  draws <- roc1_draws(m, newdata)

  draws |>
    pull(p_hit) |>
    between(0, 1) |>
    all() |>
    expect_equal(TRUE)

  draws |>
    pull(p_fa) |>
    between(0, 1) |>
    all() |>
    expect_equal(TRUE)

  expect_equal(draws, add_roc1_draws(newdata, m))

  ## compare between _draws and _rvars
  draws |>
    median_qi(p_hit) |>
    pull(p_hit) |>
    near(roc1_rvars(m, newdata) |>
           median_qi(p_hit) |>
           pull(p_hit),
         tol=.01) |>
    expect_all_true()

  draws |>
    median_qi(p_fa) |>
    pull(p_fa) |>
    near(roc1_rvars(m, newdata) |>
           median_qi(p_fa) |>
           pull(p_fa),
         tol=.01) |>
    expect_all_true()
})

test_that("roc2_draws works", {
  draws <- roc2_draws(m, newdata)

  draws |>
    pull(p_hit2) |>
    between(0, 1) |>
    all() |>
    expect_equal(TRUE)

  draws |>
    pull(p_fa2) |>
    between(0, 1) |>
    all() |>
    expect_equal(TRUE)

  expect_equal(draws, add_roc2_draws(newdata, m))

  ## compare between _draws and _rvars
  draws |>
    median_qi(p_hit2) |>
    pull(p_hit2) |>
    near(roc2_rvars(m, newdata) |>
           median_qi(p_hit2) |>
           pull(p_hit2),
         tol=.01) |>
    expect_all_true()

  draws |>
    median_qi(p_fa2) |>
    pull(p_fa2) |>
    near(roc2_rvars(m, newdata) |>
           median_qi(p_fa2) |>
           pull(p_fa2),
         tol=.01) |>
    expect_all_true()
})
