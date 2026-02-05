#' Given the distances between successive confidence thresholds,
#' calculate the average of the cumulative distances to 0.
#' @param ... a series of distances between confidence thresholds
metacognitive_bias <- function(...) {
  k <- length(c(...))
  sum(c(...) * (k:1) / k)
}

#' Get draws of `meta-delta`, an index of metacognitive bias.
#' @param object the `brms` model with the `metad` family
#' @param newdata Data frame from which to generate estimates
#' @param ... additional parameters to pass to `tidybayes::linpred_draws`
#' @param by_response If `TRUE`, generate separate estimates for each response.
#' @export
metacognitive_bias_draws <- function(object, newdata, ..., by_response = TRUE) {
  dpar <- object$family$dpar[stringr::str_starts(object$family$dpar, "metac2")]
  draws <- tidybayes::linpred_draws(object, newdata, ..., dpar = dpar, transform = TRUE)

  ## grouping columns
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", ".draw"))]

  draws <- draws |>
    group_by(!!!.cols) |>
    select(".draw", !!!.cols, starts_with("metac2")) |>
    tidyr::pivot_longer(starts_with("metac2"),
      names_to = c("response", "confidence"),
      names_pattern = "metac2([[:alpha:]]*)([[:digit:]])diff"
    ) |>
    mutate(response = as.integer(.data$response == "one")) |>
    group_by(!!!.cols, .data$response, .data$.draw) |>
    summarize(metacognitive_bias = metacognitive_bias(.data$value))

  if (!by_response) {
    draws <- draws |>
      group_by(!!!.cols, .data$.draw) |>
      summarize(metacognitive_bias = mean(.data$metacognitive_bias))
  }

  draws
}

#' Get draws of `meta-delta`, an index of metacognitive bias.
#' @param newdata Data frame from which to generate estimates
#' @param object the `brms` model with the `metad` family
#' @param ... additional parameters to pass to `tidybayes::linpred_draws`
#' @param by_response If `TRUE`, generate separate estimates for each response.
#' @export
add_metacognitive_bias_draws <- function(newdata, object, ..., by_response = TRUE) {
  metacognitive_bias_draws(object, newdata, ..., by_response = by_response)
}
