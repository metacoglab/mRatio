#' Obtain posterior draws of mean confidence separately for each possible stimulus
#' @param object the `brms` model with the `metad` family
#' @param newdata Data frame from which to generate posterior predictions
#' @param ... Additional arguments to tidybayes::epred_draws
#' @param by_stimulus If TRUE, predict mean confidence separately by stimulus
#' @param by_response If TRUE, predict mean confidence separately by response
#' Otherwise, predict mean confidence averaging across stimuli.
#' @returns a tibble containing posterior draws of mean confidence with the following
#' columns:
#'   .row: the row of `newdata`
#'   .chain, .iteration, .draw: identifiers for the posterior sample
#'   stimulus: indicator for stimulus presence
#'   .epred: the predicted mean confidence
#' @export
mean_confidence_draws <- function(object, newdata, ...,
                                  by_stimulus = TRUE, by_response = TRUE) {
  draws <- tidybayes::epred_draws(object, newdata, ...)

  ## number of confidence levels
  K <- as.integer(n_distinct(draws$.category) / 4)

  ## grouping columns
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", "stimulus", ".draw"))]

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
    )

  if (by_stimulus) {
    if (by_response) {
      draws |>
        group_by(
          .data$.row, .data$.chain, .data$.iteration, .data$.draw,
          .data$stimulus, .data$response, !!!syms(.cols)
        ) |>
        mutate(.epred = .data$.epred / sum(.data$.epred)) |> ## normalize within responses
        summarize(.epred = sum(.data$.epred * .data$confidence), .groups = "keep") |>
        group_by(.data$.row, .data$stimulus, .data$response, !!!syms(.cols))
    } else {
      draws |>
        group_by(
          .data$.row, .data$.chain, .data$.iteration, .data$.draw,
          .data$stimulus, !!!syms(.cols)
        ) |>
        summarize(.epred = sum(.data$.epred * .data$confidence), .groups = "keep") |>
        group_by(.data$.row, .data$stimulus, !!!syms(.cols))
    }
  } else {
    if (by_response) {
      draws |>
        group_by(
          .data$.row, .data$.chain, .data$.iteration, .data$.draw,
          !!!syms(.cols), .data$response
        ) |>
        mutate(.epred = .data$.epred / sum(.data$.epred)) |>
        group_by(
          .data$.row, .data$.chain, .data$.iteration, .data$.draw,
          !!!syms(.cols), .data$response, .data$confidence
        ) |>
        mutate(.epred = .data$confidence * sum(.data$.epred)) |>
        group_by(
          .data$.row, .data$.chain, .data$.iteration, .data$.draw,
          !!!syms(.cols), .data$response
        ) |>
        summarize(.epred = sum(.data$.epred) / 2, .groups = "keep") |>
        group_by(.data$.row, .data$response, !!!syms(.cols))
    } else {
      draws |>
        group_by(
          .data$.row, .data$.chain, .data$.iteration, .data$.draw,
          !!!syms(.cols)
        ) |>
        summarize(.epred = sum(.data$.epred * .data$confidence) / 2, .groups = "keep") |>
        group_by(.data$.row, !!!syms(.cols))
    }
  }
}

#' Obtain posterior draws of mean confidence separately for each possible stimulus
#' @param newdata Data frame from which to generate posterior predictions
#' @param object The `brms` model with the `metad` family
#' @param ... Additional parameters passed to `tidybayes::epred_draws`
#' @returns a tibble containing posterior draws of mean confidence with the following
#' columns:
#'   .row: the row of `newdata`
#'   .chain, .iteration, .draw: identifiers for the posterior sample
#'   stimulus: indicator for stimulus presence
#'   .epred: the predicted mean confidence
#' @export
add_mean_confidence_draws <- function(newdata, object, ...) {
  mean_confidence_draws(object, newdata, ...)
}
