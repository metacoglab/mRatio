#' Obtain posterior draws of the pseudo type 1 receiver operating characteristic (ROC) curve.
#'
#' @description Given a data frame and a meta-d' model, adds estimates of the
#' cumulative probability over joint_responses.
#' For `roc1_draws` and `add_roc1_draws`, estimates are returned in a tidy
#' tibble with one row per posterior draw and per joint response.
#' For `roc1_rvars` and `add_roc1_rvars`, parameters are returned as
#' [posterior::rvar]s, with one row per row in `newdata` and per joint response.
#'
#' @param object The `brms` model with the `metad` family
#' @param newdata A data frame from which to generate posterior predictions
#' @param ... Additional parameters passed to [tidybayes::epred_draws] or [tidybayes::epred_rvars]
#' @param bounds If `TRUE`, include the endpoints of the ROC at \eqn{(0, 0)} and \eqn{(1, 1)}.
#' Otherwise, the endpoints are excluded.
#' @returns a tibble containing posterior draws of the pseudo type 1 ROC with the following
#' columns:
#'  * `.row`: the row of `newdata`
#'  * `.chain`, `.iteration`, `.draw`: for `roc1_draws` and `add_roc1_draws`, identifiers for the posterior sample
#'  * `joint_response`: the combined type 1 / type 2 response (\eqn{J \in [1, 2K]}) for \eqn{K} confidence levels)
#'  * `response`: the type 1 response for perceived stimulus presence (\eqn{R \in \{0, 1\}})
#'  * `confidence`: the type 2 confidence response (\eqn{C \in [1, K]})
#'  * `p_fa`: the cumulative probability of a 'present'/'old' response for `stimulus==0` (\eqn{P(J \ge j \;\vert\; S=0)})
#'  * `p_hit`: the cumulative probability of a 'present'/'old' response for `stimulus==1` (\eqn{P(J \ge j \;\vert\; S=1)})
#' @rdname roc1_draws
#' @examples
#' # running few iterations so example runs quickly, use more in practice
#' m <- fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#' newdata <- tidyr::tibble(.row = 1)
#'
#' # compute pseudo-type 1 ROC curve
#' roc1_draws(m, newdata)
#' add_roc1_draws(newdata, m)
#'
#' # use posterior::rvar for additional efficiency
#' roc1_rvars(m, newdata)
#' add_roc1_draws(newdata, m)
#'
#' # include the ROC bounds
#' roc1_draws(m, newdata, bounds = TRUE)
#' @export
roc1_draws <- function(object, newdata, ..., bounds = FALSE) {
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

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
      response = type1_response(.data$joint_response, K),
      confidence = type2_response(.data$joint_response, K)
    ) |>
    filter(.data$joint_response < 2 * K) |>
    group_by(.data$.row, .data$stimulus, .data$.draw) |>
    mutate(.epred = 1 - cumsum(.data$.epred)) |>
    select(-".category") |>
    tidyr::pivot_wider(
      names_from = "stimulus", values_from = ".epred",
      names_prefix = "p_"
    ) |>
    rename(p_hit = "p_1", p_fa = "p_0") |>
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

#' @rdname roc1_draws
#' @export
add_roc1_draws <- function(newdata, object, ...) {
  roc1_draws(object, newdata, ...)
}

#' @rdname roc1_draws
#' @export
roc1_rvars <- function(object, newdata, ..., bounds = FALSE) {
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

  draws <- tidybayes::epred_rvars(
    object = object, newdata = newdata, ...,
    columns_to = ".category"
  )

  ## number of confidence levels
  K <- as.integer(n_distinct(draws$.category) / 4)

  ## grouping columns
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", "joint_response", "response", "confidence"))]

  draws <- draws |>
    tidyr::separate_wider_delim(.data$.category,
      delim = "_",
      names = c(NA, "stimulus", "joint_response")
    ) |>
    mutate(
      stimulus = as.integer(.data$stimulus),
      joint_response = as.integer(.data$joint_response),
      response = type1_response(.data$joint_response, K),
      confidence = type2_response(.data$joint_response, K)
    ) |>
    filter(.data$joint_response < 2 * K) |>
    group_by(.data$.row, .data$stimulus) |>
    mutate(.epred = 1 - cumsum(.data$.epred)) |>
    tidyr::pivot_wider(
      names_from = "stimulus", values_from = ".epred",
      names_prefix = "p_"
    ) |>
    rename(p_hit = "p_1", p_fa = "p_0") |>
    group_by(
      .data$.row, !!!syms(.cols), .data$joint_response,
      .data$response, .data$confidence
    )

  if (bounds) {
    ## add (0, 0) and (1, 1) points to ROC
    draws <- draws |>
      bind_rows(draws |>
        ungroup() |>
        distinct(.data$.row, !!!syms(.cols)) |>
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

#' @rdname roc1_draws
#' @export
add_roc1_rvars <- function(newdata, object, ...) {
  roc1_rvars(object, newdata, ...)
}


#' Obtain posterior draws of the response-specific type 2 receiver operating characteristic (ROC) curves.
#'
#' @description Given a data frame and a meta-d' model, adds estimates of the
#' cumulative probability over confidence for each type 1 response.
#' For `roc2_draws` and `add_roc2_draws`, estimates are returned in a tidy
#' tibble with one row per posterior draw and per joint response.
#' For `roc2_rvars` and `add_roc2_rvars`, parameters are returned as
#' `posterior::rvar`s, with one row per row in `newdata` and per joint response.
#'
#' @param object The `brms` model with the `metad` family
#' @param newdata A data frame from which to generate posterior predictions
#' @param ... Additional parameters passed to [tidybayes::epred_draws]
#' @param bounds If `TRUE`, include the endpoints of the ROC at \eqn{(0, 0)} and \eqn{(1, 1)}.
#' Otherwise, the endpoints are excluded.
#' @returns a tibble containing posterior draws of the pseudo type 1 ROC with the following
#' columns:
#'  * `.row`: the row of `newdata`
#'  * `.chain`, `.iteration`, `.draw`: for `roc2_draws` and `add_roc2_draws`, identifiers for the posterior sample
#'  * `response`: the type 1 response for perceived stimulus presence (\eqn{R \in \{0, 1\}})
#'  * `confidence`: the type 2 confidence response (\eqn{C \in [1, K]})
#'  * `p_fa2`: the cumulative probability of an incorrect response (\eqn{P(C\ge c \;\vert\; R\ne S)})
#'  * `p_hit2`: the cumulative probability of a correct response (\eqn{P(C\ge c \;\vert\; R = S)})
#' @rdname roc2_draws
#' @examples
#' # running few iterations so example runs quickly, use more in practice
#' m <- fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#' newdata <- tidyr::tibble(.row = 1)
#'
#' # compute type 2 ROC curve
#' roc2_draws(m, newdata)
#' add_roc2_draws(newdata, m)
#'
#' # use posterior::rvar for additional efficiency
#' roc2_rvars(m, newdata)
#' add_roc2_rvars(newdata, m)
#'
#' # include the ROC bounds
#' roc2_draws(m, newdata, bounds = TRUE)
#' @export
roc2_draws <- function(object, newdata, ..., bounds = FALSE) {
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

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
      response = type1_response(.data$joint_response, K),
      confidence = type2_response(.data$joint_response, K),
      accuracy = as.integer(.data$stimulus == .data$response)
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
    rename(p_hit2 = "p_1", p_fa2 = "p_0") |>
    group_by(.data$.row, !!!syms(.cols), .data$response, .data$confidence)

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

#' @rdname roc2_draws
#' @export
add_roc2_draws <- function(newdata, object, ...) {
  roc2_draws(object, newdata, ...)
}

#' @rdname roc2_draws
#' @export
roc2_rvars <- function(object, newdata, ..., bounds = FALSE) {
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

  draws <- tidybayes::epred_rvars(
    object = object, newdata = newdata, ...,
    columns_to = ".category"
  )

  ## number of confidence levels
  K <- as.integer(n_distinct(draws$.category) / 4)

  ## grouping columns
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", "response", "confidence"))]

  draws <- draws |>
    tidyr::separate_wider_delim(.data$.category,
      delim = "_",
      names = c(NA, "stimulus", "joint_response")
    ) |>
    mutate(
      stimulus = as.integer(.data$stimulus),
      joint_response = as.integer(.data$joint_response),
      response = type1_response(.data$joint_response, K),
      confidence = type2_response(.data$joint_response, K),
      accuracy = as.integer(.data$stimulus == .data$response)
    ) |>
    group_by(.data$.row, .data$accuracy, .data$response) |>
    mutate(
      .epred = cumsum(.data$.epred) / posterior::rvar_sum(.data$.epred),
      .epred = rvar_ifelse(.data$response == 1, 1 - .data$.epred, .data$.epred)
    ) |>
    filter(
      !(.data$response == 0 & .data$confidence == 1),
      !(.data$response == 1 & .data$confidence == K)
    ) |>
    select(-"joint_response", -"stimulus") |>
    ungroup() |>
    tidyr::pivot_wider(
      names_from = "accuracy",
      values_from = ".epred", names_prefix = "p_"
    ) |>
    rename(p_hit2 = "p_1", p_fa2 = "p_0") |>
    group_by(.data$.row, !!!syms(.cols), .data$response, .data$confidence) |>
    arrange(.data$.row, !!!syms(.cols), .data$response, .data$confidence)

  if (bounds) {
    ## add (0, 0) and (1, 1) points to ROC
    draws <- draws |>
      bind_rows(draws |>
        ungroup() |>
        distinct(.data$.row, !!!syms(.cols)) |>
        tidyr::expand_grid(tibble(
          response = c(0, 0, 1, 1),
          confidence = c(1, K + 1, 0, K),
          p_fa2 = c(1, 0, 1, 0),
          p_hit2 = c(1, 0, 1, 0)
        )))
  }

  draws
}

#' @rdname roc2_draws
#' @export
add_roc2_rvars <- function(newdata, object, ...) {
  roc2_rvars(object, newdata, ...)
}
