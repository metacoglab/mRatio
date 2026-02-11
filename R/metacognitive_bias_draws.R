#' Given the distances between successive confidence thresholds,
#' calculate the average of the cumulative distances to 0.
#' @param ... a series of distances between confidence thresholds
#' @param rvar if `TRUE`, use `posterior::rvar_sum` in place of `sum`
#' @keywords internal
metacognitive_bias <- function(..., rvar = FALSE) {
  k <- length(c(...))

  if (rvar) {
    rvar_sum(c(...) * (k:1) / k)
  } else {
    sum(c(...) * (k:1) / k)
  }
}

#' Obtain posterior draws of an index of metacognitive bias
#'
#' @description Computes \eqn{\textrm{meta-}\Delta}, an index of metacognitive
#'   bias. \eqn{\textrm{meta-}\Delta} is the distance between `meta_c` and the
#'   average of the the confidence criteria `meta_c2_0` and `meta_c2_1`. For
#'   `metacognitive_bias_draws` and `add_metacognitive_bias_draws`, parameters
#'   are returned in a tidy tibble with one row per posterior draw and per
#'   response. For `metacognitive_bias_rvars` and
#'   `add_metacognitive_bias_rvars`, parameters are returned as
#'   [posterior::rvar]s, with one row per row in `newdata` and per response.
#'
#' @param object The `brms` model with the `metad` family
#' @param newdata A data frame from which to generate posterior predictions
#' @param ... Additional parameters passed to [tidybayes::epred_draws] or [tidybayes::epred_rvars]
#' @param by_response If `TRUE`, compute metacognitive bias separately for the
#'   two type 1 responses. If `FALSE`, compute an un-weighted average of the two
#'   measures.
#' @returns a tibble containing posterior draws of \eqn{\textrm{meta-}\Delta}
#'   with the following columns:
#'  * `.row`: the row of `newdata`
#'  * `.chain`, `.iteration`, `.draw`: for `metacognitive_bias_draws` and `add_metacognitive_bias_draws`, identifiers for the posterior sample
#'  * `response`: the type 1 response for perceived stimulus presence
#'  * `metacognitive_bias`: the distance between `meta_c` and the average of
#'   the confidence criteria `meta_c2_{response}`.
#' @rdname bias_draws
#' @examples
#' # running few iterations so example runs quickly, use more in practice
#' m <- fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#' newdata <- tidyr::tibble(.row = 1)
#'
#' # compute metacognitive bias
#' metacognitive_bias_draws(m, newdata)
#' add_metacognitive_bias_draws(newdata, m)
#'
#' # use posterior::rvar for increased efficiency
#' metacognitive_bias_rvars(m, newdata)
#' add_metacognitive_bias_rvars(newdata, m)
#'
#' # average over the two type 1 responses
#' metacognitive_bias_draws(m, newdata, by_response = FALSE)
#' metacognitive_bias_rvars(m, newdata, by_response = FALSE)
#' @export
metacognitive_bias_draws <- function(object, newdata, ..., by_response = TRUE) {
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

  dpar <- object$family$dpar[stringr::str_starts(object$family$dpar, "metac2")]
  draws <- tidybayes::linpred_draws(object, newdata, ..., dpar = dpar, transform = TRUE)

  ## grouping columns
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", ".draw"))]

  draws <- draws |>
    group_by(.data$.row, !!!syms(.cols)) |>
    select(
      ".row", !!!syms(.cols),
      ".chain", ".iteration", ".draw", starts_with("metac2")
    ) |>
    tidyr::pivot_longer(starts_with("metac2"),
      names_to = c("response", "confidence"),
      names_pattern = "metac2([[:alpha:]]*)([[:digit:]])diff"
    ) |>
    mutate(response = as.integer(.data$response == "one")) |>
    group_by(
      .data$.row, !!!syms(.cols), .data$response,
      .data$.chain, .data$.iteration, .data$.draw
    ) |>
    summarize(metacognitive_bias = metacognitive_bias(.data$value)) |>
    group_by(.data$.row, !!!syms(.cols), .data$response)

  if (!by_response) {
    draws <- draws |>
      group_by(
        .data$.row, !!!syms(.cols),
        .data$.chain, .data$.iteration, .data$.draw
      ) |>
      summarize(metacognitive_bias = mean(.data$metacognitive_bias)) |>
      group_by(.data$.row, !!!syms(.cols))
  }

  draws
}

#' @rdname bias_draws
#' @export
add_metacognitive_bias_draws <- function(newdata, object, ...) {
  metacognitive_bias_draws(object, newdata, ...)
}


#' @rdname bias_draws
#' @export
metacognitive_bias_rvars <- function(object, newdata, ..., by_response = TRUE) {
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

  dpar <- object$family$dpar[stringr::str_starts(object$family$dpar, "metac2")]
  draws <- tidybayes::linpred_rvars(object, newdata, ..., dpar = dpar, transform = TRUE)

  ## grouping columns
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", ".draw"))]

  draws <- draws |>
    group_by(.data$.row, !!!syms(.cols)) |>
    select(".row", !!!syms(.cols), starts_with("metac2")) |>
    tidyr::pivot_longer(starts_with("metac2"),
      names_to = c("response", "confidence"),
      names_pattern = "metac2([[:alpha:]]*)([[:digit:]])diff"
    ) |>
    mutate(response = as.integer(.data$response == "one")) |>
    group_by(.data$.row, !!!syms(.cols), .data$response) |>
    summarize(
      metacognitive_bias = metacognitive_bias(.data$value, rvar = TRUE),
      .groups = "keep"
    )

  if (!by_response) {
    draws <- draws |>
      group_by(.data$.row, !!!syms(.cols)) |>
      summarize(metacognitive_bias = posterior::rvar_mean(.data$metacognitive_bias))
  }

  draws
}

#' @rdname bias_draws
#' @export
add_metacognitive_bias_rvars <- function(newdata, object, ...) {
  metacognitive_bias_rvars(object, newdata, ...)
}
