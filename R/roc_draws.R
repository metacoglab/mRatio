#' Obtain posterior draws of the pseudo type 1 receiver operating characteristic (ROC) curve.
#' @param object the `brms` model with the `metad` family
#' @param newdata Data frame from which to generate posterior predictions
#' @param ... Additional parameters passed to `tidybayes::epred_draws`
#' @param bounds If `TRUE`, include the endpoints of the ROC at `(0, 0)` and `(1, 1)`.
#' @returns a tibble containing posterior draws of the pseudo type 1 ROC with the following
#' columns:
#'   .row: the row of `newdata`
#'   .chain, .iteration, .draw: identifiers for the posterior sample
#'   joint_response: the combined type 1 / type 2 response (in 1:2*K for K confidence levels)
#'   response: the type 1 response for perceived stimulus presence
#'   confidence: the type 2 confidence response
#'   p_fa: the cumulative probability of a 'present'/'old' response for stimulus==0
#'   p_hit: the cumulative probability of a 'present'/'old' response for stimulus==1
#' @export
roc1_draws <- function(object, newdata, ..., bounds = FALSE) {
  draws <- tidybayes::epred_draws(object = object, newdata = newdata, ...)

  ## number of confidence levels
  K <- as.integer(n_distinct(draws$.category) / 4)

  ## grouping columns
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", "joint_response", "response", "confidence"))]

  draws <- draws |>
    mutate(
      .category = as.integer(.data$.category),
      stimulus = as.integer(.data$.category > 2 * K),
      joint_response = ifelse(.data$stimulus,
        .data$.category - 2 * K,
        .data$.category
      ),
      response = as.integer(.data$joint_response > K),
      confidence = ifelse(.data$joint_response > K,
        .data$joint_response - K,
        K + 1 - .data$joint_response
      )
    ) |>
    filter(.data$joint_response < 2 * K) |>
    group_by(.data$.row, .data$stimulus, .data$.draw) |>
    mutate(.epred = 1 - cumsum(.data$.epred)) |>
    select(-".category") |>
    tidyr::pivot_wider(
      names_from = "stimulus", values_from = ".epred",
      names_prefix = "p_"
    ) |>
    rename(p_hit = .data$p_1, p_fa = .data$p_0) |>
    group_by(
      .data$.row, !!!syms(.cols), .data$joint_response,
      .data$response, .data$confidence
    )

  if (bounds) {
    ## add (0, 0) and (1, 1) points to ROC
    draws <- draws |>
      bind_rows(draws |>
        ungroup() |>
        distinct(
          .data$.row, !!!syms(.cols),
          .data$.chain, .data$.iteration, .data$.draw
        ) |>
        tidyr::expand_grid(tibble(
          joint_response = c(0, K * 2),
          response = c(0, 1),
          confidence = c(K + 1, K),
          p_fa = c(1, 0),
          p_hit = c(1, 0)
        ))) |>
      arrange(.data$joint_response)
  }

  draws
}

#' Obtain posterior draws of the pseudo type 1 receiver operating characteristic (ROC) curve.
#' @param newdata Data frame from which to generate posterior predictions
#' @param object the `brms` model with the `metad` family
#' @param ... Additional parameters passed to `tidybayes::epred_draws`
#' @param bounds If `TRUE`, include the endpoints of the ROC at `(0, 0)` and `(1, 1)`.
#' @returns a tibble containing posterior draws of the pseudo type 1 ROC with the following
#' columns:
#'   .row: the row of `newdata`
#'   .chain, .iteration, .draw: identifiers for the posterior sample
#'   joint_response: the combined type 1 / type 2 response (in 1:2*K for K confidence levels)
#'   response: the type 1 response for perceived stimulus presence
#'   confidence: the type 2 confidence response
#'   p_fa: the cumulative probability of a 'present'/'old' response for stimulus==0
#'   p_hit: the cumulative probability of a 'present'/'old' response for stimulus==1
#' @export
add_roc1_draws <- function(newdata, object, ..., bounds = FALSE) {
  roc1_draws(object, newdata, ..., bounds = bounds)
}

#' Obtain posterior draws of the response-specific type 2 receiver operating characteristic (ROC) curves.
#' @param object the `brms` model with the `metad` family
#' @param newdata Data frame from which to generate posterior predictions
#' @param ... Additional parameters passed to `tidybayes::epred_draws`
#' @param bounds If `TRUE`, include the endpoints of the ROC at `(0, 0)` and `(1, 1)`.
#' @returns a tibble containing posterior draws of the response-specific type 2 ROCs with the following
#' columns:
#'   .row: the row of `newdata`
#'   .chain, .iteration, .draw: identifiers for the posterior sample
#'   response: the type 1 response for perceived stimulus presence
#'   confidence: the type 2 confidence response
#'   p_fa2: the cumulative probability of an incorrect but confident response
#'   p_hit2: the cumulative probability of a correct and confident response
#' @export
roc2_draws <- function(object, newdata, ..., bounds = FALSE) {
  draws <- tidybayes::epred_draws(object = object, newdata = newdata, ...)

  ## number of confidence levels
  K <- as.integer(n_distinct(draws$.category) / 4)

  ## grouping columns
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", "response", "confidence"))]

  draws <- draws |>
    mutate(
      .category = as.integer(.data$.category),
      stimulus = as.integer(.data$.category > 2 * K),
      joint_response = ifelse(.data$stimulus,
        .data$.category - 2 * K,
        .data$.category
      ),
      response = as.integer(.data$joint_response > K),
      accuracy = as.integer(.data$stimulus == .data$response),
      confidence = ifelse(.data$joint_response > K,
        .data$joint_response - K,
        K + 1 - .data$joint_response
      )
    ) |>
    group_by(.data$.row, .data$.draw, .data$accuracy, .data$response) |>
    mutate(
      .epred = cumsum(.data$.epred) / sum(.data$.epred),
      .epred = ifelse(.data$response, 1 - .data$.epred, .data$.epred)
    ) |>
    filter(
      !(.data$response == 0 & .data$confidence == 1),
      !(.data$response == 1 & .data$confidence == K)
    ) |>
    select(-".category", -"joint_response", -"stimulus") |>
    ungroup() |>
    tidyr::pivot_wider(
      names_from = "accuracy",
      values_from = ".epred", names_prefix = "p_"
    ) |>
    rename(p_hit2 = .data$p_1, p_fa2 = .data$p_0) |>
    group_by(.data$.row, .data$response, .data$confidence, !!!syms(.cols))

  if (bounds) {
    ## add (0, 0) and (1, 1) points to ROC
    draws <- draws |>
      bind_rows(draws |>
        ungroup() |>
        distinct(
          .data$.row, !!!syms(.cols),
          .data$.chain, .data$.iteration, .data$.draw
        ) |>
        tidyr::expand_grid(tibble(
          response = c(0, 0, 1, 1),
          confidence = c(1, K + 1, 0, K),
          p_fa2 = c(1, 0, 1, 0),
          p_hit2 = c(1, 0, 1, 0)
        )))
  }

  draws
}

#' Obtain posterior draws of the response-specific type 2 receiver operating characteristic (ROC) curves.
#' @param newdata Data frame from which to generate posterior predictions
#' @param object the `brms` model with the `metad` family
#' @param ... Additional parameters passed to `tidybayes::epred_draws`
#' @param bounds If `TRUE`, include the endpoints of the ROC at `(0, 0)` and `(1, 1)`.
#' @returns a tibble containing posterior draws of the response-specific type 2 ROCs with the following
#' columns:
#'   .row: the row of `newdata`
#'   .chain, .iteration, .draw: identifiers for the posterior sample
#'   response: the type 1 response for perceived stimulus presence
#'   confidence: the type 2 confidence response
#'   p_fa2: the cumulative probability of an incorrect but confident response
#'   p_hit2: the cumulative probability of a correct and confident response
#' @export
add_roc2_draws <- function(newdata, object, ..., bounds = FALSE) {
  roc2_draws(object, newdata, ..., bounds = bounds)
}
