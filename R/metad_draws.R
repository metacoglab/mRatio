#' Obtain posterior draws of meta-d' model parameters
#'
#' @description Given a data frame and a meta-d' model, adds estimates of all
#'   model parameters. For `metad_draws` and `add_metad_draws`, parameters are
#'   returned in a tidy tibble with one row per posterior draw. For
#'   `metad_rvars` and `add_metad_rvars`, parameters are returned as
#'   [posterior::rvar]s, with one row per row in `newdata`.
#'
#' @param object The `brms` model with the `metad` family
#' @param newdata A data frame from which to generate posterior predictions
#' @param ... Additional arguments passed to [tidybayes::add_linpred_draws] or
#'   [tidybayes::add_linpred_rvars]
#' @param pivot_longer Return the draws in long format?
#' * if `TRUE`, resulting data frame has one row per posterior draw per model parameter
#' * if `FALSE` (default), resulting data frame has one row per posterior draw
#' @returns a tibble containing posterior draws of model parameters with the
#'   following columns:
#'  * `.row`: the row of `newdata`
#'  * `.chain`, `.iteration`, `.draw`: for `metad_draws`, identifiers for the posterior sample
#'  * `.variable`, `.value`: if `pivot_longer=TRUE`, `.variable` identifies different meta-d' model parameters and `.value` stores posterior samples
#'  * `M`, `dprime`, `c`, `meta_dprime`, `meta_c`, `meta_c2_0_<k>`, `meta_c2_1_<k>`: if `pivot_longer=FALSE`, posterior samples of all meta-d' model parameters
#' @examples
#' # running few iterations so example runs quickly, use more in practice
#' m <- fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#' newdata <- tidyr::tibble(.row = 1)
#'
#' # obtain model parameters (wide format)
#' metad_draws(m, newdata)
#' add_metad_draws(newdata, m)
#'
#' # obtain model parameters (long format)
#' metad_draws(m, newdata, pivot_longer = TRUE)
#' add_metad_draws(newdata, m, pivot_longer = TRUE)
#'
#' # obtain model parameters (wide format, posterior::rvar)
#' metad_rvars(m, newdata)
#' add_metad_rvars(newdata, m)
#'
#' # obtain model parameters (long format, posterior::rvar)
#' metad_rvars(m, newdata, pivot_longer = TRUE)
#' add_metad_rvars(newdata, m, pivot_longer = TRUE)
#'
#' @rdname metad_draws
#' @export
metad_draws <- function(object, newdata, ..., pivot_longer = FALSE) {
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

  ## grouping columns
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", "stimulus", ".draw"))]

  draws <- object |>
    tidybayes::linpred_draws(newdata, ..., value = "M", dpar = TRUE, transform = TRUE) |>
    select(-"mu") |>
    mutate(
      meta_dprime = .data$M * .data$dprime,
      meta_c = ifelse(get_metac(object) == "absolute",
        .data$c,
        .data$M * .data$c
      )
    ) |>
    tidyr::pivot_longer(starts_with("metac2"),
      names_pattern = "metac2(zero|one)([[:digit:]*])diff",
      names_to = c("response", "k"),
      values_to = "diff"
    ) |>
    mutate(response = as.numeric(.data$response == "one")) |>
    group_by(
      .data$.row, !!!syms(.cols),
      .data$.chain, .data$.iteration, .data$.draw, .data$response
    ) |>
    mutate(c2 = ifelse(.data$response, .data$meta_c + cumsum(.data$diff),
      .data$meta_c - cumsum(.data$diff)
    )) |>
    ungroup() |>
    select(-"diff") |>
    tidyr::pivot_wider(
      names_from = c("response", "k"), values_from = "c2",
      names_prefix = "meta_c2_"
    )

  if (pivot_longer) {
    draws |>
      tidyr::pivot_longer(
        c(
          "M", "dprime", "c", "meta_dprime",
          "meta_c", starts_with("meta_c2")
        ),
        names_to = ".variable", values_to = ".value"
      ) |>
      group_by(.data$.row, !!!syms(.cols), .data$.variable)
  } else {
    draws |>
      group_by(.data$.row, !!!syms(.cols))
  }
}

#' @rdname metad_draws
#' @export
add_metad_draws <- function(newdata, object, ..., pivot_longer = FALSE) {
  metad_draws(object, newdata, ..., pivot_longer = pivot_longer)
}

#' @rdname metad_draws
#' @export
metad_rvars <- function(object, newdata, ..., pivot_longer = FALSE) {
  if (object$family$family != "custom" ||
    !stringr::str_starts(object$family$name, "metad")) {
    stop("Model must use the `metad` family.")
  }

  ## grouping columns
  .cols <- names(newdata)
  .cols <- .cols[!(.cols %in% c(".row", "stimulus", ".draw"))]

  draws <- object |>
    tidybayes::linpred_rvars(newdata, ..., value = "M", dpar = TRUE, transform = TRUE) |>
    select(-"mu") |>
    mutate(
      meta_dprime = .data$M * .data$dprime,
      meta_c = .data$c
    ) |>
    tidyr::pivot_longer(starts_with("metac2"),
      names_pattern = "metac2(zero|one)([[:digit:]*])diff",
      names_to = c("response", "k"),
      values_to = "diff"
    ) |>
    mutate(response = as.numeric(.data$response == "one")) |>
    group_by(
      .data$.row, ## !!!syms(.cols),
      .data$response
    ) |>
    mutate(c2 = posterior::rvar_ifelse(
      .data$response == 1,
      .data$meta_c + cumsum(.data$diff),
      .data$meta_c - cumsum(.data$diff)
    )) |>
    ungroup() |>
    select(-"diff") |>
    tidyr::pivot_wider(
      names_from = c("response", "k"), values_from = "c2",
      names_prefix = "meta_c2_"
    )

  if (pivot_longer) {
    draws |>
      tidyr::pivot_longer(
        c(
          "M", "dprime", "c", "meta_dprime",
          "meta_c", starts_with("meta_c2")
        ),
        names_to = ".variable", values_to = ".value"
      ) |>
      group_by(.data$.row, !!!syms(.cols), .data$.variable)
  } else {
    draws |>
      group_by(.data$.row, !!!syms(.cols))
  }
}

#' @rdname metad_draws
#' @export
add_metad_rvars <- function(newdata, object, pivot_longer = FALSE) {
  metad_rvars(object, newdata, pivot_longer = pivot_longer)
}
