#' Convert binary variable `x` from `{0, 1}` to `{-1, 1}`
#' @param x a binary variable interpretable as `0` or `1`
#' @returns `1` if `x==1`, else `-1`
#' @rdname signed
#' @export
to_signed <- function(x) ifelse(x, 1, -1)

#' Convert binary variable `x` from `{-1, 1}` to `{0, 1}`
#' @param x a binary variable interpretable as `-1` or `1`
#' @returns `1` if `x==1`, else `0`
#' @rdname signed
#' @export
to_unsigned <- function(x) as.numeric(x > 0)

#' Convert a type 1 response (0 or 1) and a corresponding confidence level
#' (between 1 and K) into a joint response between 1 and 2*K, with outer
#' values reflecting confident responses and intermediate values reflecting
#' uncertainty.
#' @param response type 1 response (0 or 1)
#' @param confidence type 2 response/confidence rating (1:K)
#' @param K maximum confidence level
#' @rdname aggregation
#' @export
joint_response <- function(response, confidence, K) {
  ifelse(response, confidence + K, K + 1 - confidence)
}

#' For a vector of counts of joint type 1/type 2 responses,
#' compute the corresponding probabilities.
#' @param x A vector of counts of joint type 1/type 2 responses
#' @returns A vector of response probabilities
#' @rdname aggregation
#' @export
response_probabilities <- function(x) {
  if ((length(x) %% 4) != 0) {
    stop(paste0("Length of response counts should be divisible by 4, but is: ", length(x)))
  }
  L <- length(x) / 2

  c(
    x[1:L] / sum(x[1:L]),
    x[(L + 1):(2 * L)] / sum(x[(L + 1):(2 * L)])
  )
}

#' Aggregate `data` by columns `response`, `confidence`,
#' and any other variables in `...`.
#'
#' @param data the tibble to aggregate
#' @param ... grouping columns in the tibble
#' @param .response the name of the column containing trial counts
#' @param K The number of confidence levels in `data`. If `NULL`, this is estimated from `data`.
#' @returns A tibble with one row per combination of the variables in `...`,
#' and another column named by the value of `.response` containing trial counts.
#' For K confidence levels, this will be an `N x K*4` matrix, such that the
#' columns represent:
#' `[N(stimulus==0, confidence==K), ..., N(stimulus==0, confidence==1),
#'  N(stimulus==0, confidence==1), ..., N(stimulus==0, confidence==K),
#'  N(stimulus==1, confidence==K), ..., N(stimulus==1, confidence==1),
#'  N(stimulus==1, confidence==1), ..., N(stimulus==1, confidence==K)]`
#' @rdname aggregation
#' @export
metad_aggregate <- function(data, ..., .response = "N", K = NULL) {
  # number of confidence levels
  if (is.null(K)) {
    K <- n_distinct(data$confidence)
  }

  ## aggregate data if non-empty
  if (nrow(data) == 0) {
    data <- tidyr::expand_grid(..., stimulus = 0:1, response = 0:1, confidence = 1:K) |>
      mutate(
        joint_response = factor(joint_response(
          .data$response,
          .data$confidence,
          K
        )),
        n = 0
      ) |>
      select(-"response", -"confidence") |>
      arrange(..., .data$stimulus, .data$joint_response)
  } else {
    data <- data |>
      mutate(
        response = as.integer(as.character(.data$response)),
        confidence = as.integer(as.character(.data$confidence)),
        joint_response = factor(
          joint_response(
            .data$response,
            .data$confidence, K
          ),
          levels = 1:(2 * K)
        ),
        stimulus = factor(.data$stimulus),
        across(c(...), factor)
      ) |>
      group_by(...) |>
      count(.data$stimulus, .data$joint_response, .drop = FALSE)
  }

  data <- data |>
    tidyr::pivot_wider(
      names_from = c("stimulus", "joint_response"),
      values_from = "n",
      names_prefix = glue::glue("{.response}_")
    ) |>
    mutate(
      "{.response}_0" := sum(c_across(starts_with(glue::glue("{.response}_0_"),
        ignore.case = FALSE
      ))),
      "{.response}_1" := sum(c_across(starts_with(glue::glue("{.response}_1_"),
        ignore.case = FALSE
      )))
    )
  # convert counts into a matrix-column
  tibble(
    select(
      data, ...,
      all_of(c(
        glue::glue("{.response}_0"),
        glue::glue("{.response}_1")
      ))
    ),
    "{.response}" := data |> ungroup() |>
      select(matches(glue::glue("{.response}_([[:digit:]]+)_([[:digit:]]+)"),
        ignore.case = FALSE
      )) |>
      as.matrix()
  )
}

#' Fit the metad' model using the `brms` package
#' @param formula A model formula for some or all parameters of the `metad` brms family.
#' To display all parameter names for a model with `K` confidence levels, use `metad(K)`.
#' @param data A tibble containing the data to fit the model. If `aggregate`==TRUE,
#' `data` should have one row per observation. If `aggregate`==FALSE, it should be aggregated
#' to have one row per cell of the design matrix, with joint
#' type 1/type 2 response counts in a matrix column.
#' @param ... Additional parameters passed to the `brm` function.
#' @param aggregate If `TRUE`, automatically aggregate `data` by the variables included in `formula`.
#' Otherwise, `data` should already be aggregated.
#' @param K The number of confidence levels. By default, this is estimated from the data.
#' @param distribution The noise distribution to use for the signal detection model
#' @param metac_absolute If `TRUE`, fix the type 2 criterion to be equal to the type 1 criterion.
#' Otherwise, equate the criteria relatively such that metac/metadprime = c/dprime.
#' @param stanvars Additional `stanvars` to pass to the model code, for example to define an alternative
#' distribution or a custom model prior.
#' @export
fit_metad <- function(formula, data, ..., aggregate = TRUE, K = NULL,
                      distribution = "normal", metac_absolute = TRUE, stanvars = NULL) {
  data.aggregated <- NULL

  # ensure formula is a brmsformula
  if (!("brmsformula" %in% attr(formula, 'class'))) {
    formula <- brms::bf(formula)
  }

  # determine response variable
  .response <- all.vars(formula$formula)[attr(terms(formula$formula), "response")]

  # aggregate data by formula terms
  if (aggregate) {
    if (is.null(K)) {
      K <- n_distinct(data$confidence)
    }

    # get a list of variables by which to aggregate
    terms <- all.vars(brms::brmsterms(brms::bf(formula, family = metad(K)))$allvars)
    terms <- syms(terms[!(terms %in% c(.response, "Intercept"))])
    data.aggregated <- metad_aggregate(data, !!!terms, .response = .response)
  } else {
    if (is.null(K)) {
      K <- ncol(pull(data, .response)) / 4
    }
    data.aggregated <- data
  }

  if (K < 2) {
    stop(glue::glue("Error: must have at least one confidence level (found {{K}})."))
  }

  # add metad stanvars to any user-defined stanvars
  sv <- brms::stanvar(
    scode = metad_stancode(K,
      distribution = distribution,
      metac_absolute = metac_absolute
    ),
    block = "functions"
  )
  if (!is.null(stanvars)) {
    sv <- sv + stanvars
  }

  brms::brm(formula, data.aggregated,
    family = metad(K, distribution = distribution, metac_absolute = metac_absolute),
    stanvars = sv, ...
  )
}
