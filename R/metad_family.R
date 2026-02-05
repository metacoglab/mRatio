#' Generate Stan code for the meta-d' model
#'
#' @param K The number of confidence levels
#' @param distribution The noise distribution to use. Should be a parameter-free
#' distribution, i.e., one that is mean-centered without additional variance/shape parameters.
#' If the distribution is not already available in stan, you must additionally provide two
#' functions to Stan (one for `<distribution>_lcdf` and one for `<distribution>_lccdf`).
#' @param metac_absolute Should the type 2 criterion (metac) be fixed to the absolute type 1 criterion (c)?
#' If `TRUE`, the model will set `metac = c`. Otherwise, it will set `metac = M * c`, such that
#' the type 2 criterion is _relatively_ equal to the type 1 criterion
#' (i.e., `meta_c/meta_d_prime = c/d_prime`)
#' @returns A single string containing Stan code defining the likelihood for the metad' model
#' with `K` confidence levels, signal distributed according to the distribution `distribution`,
#' and where metac = c if metac_absolute is true, and metac = M*c otherwise.
metad_stancode <- function(K, distribution = "normal", metac_absolute = TRUE) {
  k <- K - 1

  dist_fun <- function(x, mean, fun = "lcdf") {
    if (distribution == "normal") {
      paste0("std_normal_", fun, "(", x, " - ", mean, ")")
    } else {
      paste0(distribution, "_", fun, "(", x, " | ", mean, ")")
    }
  }

  paste0(
    "	// Convert a binary int x from {0, 1} to {-1, 1}
	int to_signed(int x) {
	  return 2*x - 1;
	}

	// P(response, confidence | stimulus) given as simplex
	// [P(resp=0, conf=K), .... P(resp=0, conf=1), P(resp=1, conf=1), ... P(resp=1, conf=K)]
	vector metad_", distribution, "_pmf(int stimulus, real d_prime, real c, real meta_d_prime, real meta_c, vector meta_c2_0, vector meta_c2_1) {
		// number of confidence levels
		int K = size(meta_c2_0)+1;

  	// type-1 response probabilities
	  real lp_1 = ", dist_fun("c", "to_signed(stimulus)*d_prime/2", "lccdf"), ";
  	real lp_0 = ", dist_fun("c", "to_signed(stimulus)*d_prime/2", "lcdf"), ";

  	// means of type-2 distributions
  	real meta_mu = to_signed(stimulus) * meta_d_prime/2;

	  vector[K] lp2_1;         // CDFs (response == 1)
  	vector[K] lp2_0;         // CDFs (response == 0)
		vector[2*K] log_theta;   // joint (type-1 x type-2) response probabilities

	  lp2_1[1] = ", dist_fun("meta_c", "meta_mu", "lccdf"), ";
  	lp2_0[1] = ", dist_fun("meta_c", "meta_mu", "lcdf"), ";
  	for (k in 2:K) {
    	lp2_1[k] = ", dist_fun("meta_c2_1[k-1]", "meta_mu", "lccdf"), ";
    	lp2_0[k] = ", dist_fun("meta_c2_0[k-1]", "meta_mu", "lcdf"), ";

			log_theta[K-k+2] = log_diff_exp(lp2_0[k-1], lp2_0[k]);
    	log_theta[K+k-1] = log_diff_exp(lp2_1[k-1], lp2_1[k]);
  	}
  	log_theta[1] = lp2_0[K];
  	log_theta[2*K] = lp2_1[K];

	  // weight by P(response|stimulus) and normalize
  	log_theta[1:K] += lp_0 - lp2_0[1];
  	log_theta[(K+1):(2*K)] += lp_1 - lp2_1[1];

	  return exp(log_theta);
	}

	real metad__", K, "__", distribution, "__", ifelse(metac_absolute, "absolute", "relative"),
    "_lpmf(array[] int Y, real M, real d_prime, real c, ",
    paste0("real z_meta_c2_0_", 1:k, collapse = ", "), ", ",
    paste0("real z_meta_c2_1_", 1:k, collapse = ", "),
    ") {
		int K = size(Y) %/% 4; // number of confidence levels

		real meta_d_prime = M * d_prime;
		real meta_c = ", ifelse(metac_absolute, "c", "M * c"), ";
		vector[K-1] meta_c2_0 = meta_c - cumulative_sum([",
    paste0("z_meta_c2_0_", 1:k, collapse = ", "),
    "]');
		vector[K-1] meta_c2_1 = meta_c + cumulative_sum([",
    paste0("z_meta_c2_1_", 1:k, collapse = ", "),
    "]');

		// use multinomial likelihood
		return multinomial_lpmf(Y[1:(2*K)] | metad_", distribution, "_pmf(0, d_prime, c,
														meta_d_prime, meta_c, meta_c2_0, meta_c2_1)) +
  		multinomial_lpmf(Y[(2*K+1):(4*K)] |  metad_", distribution, "_pmf(1, d_prime, c,
											 meta_d_prime, meta_c, meta_c2_0, meta_c2_1));
	}"
  )
}

#' Get the R function for the model's underlying distribution functions
#' @param model The `brms` model to get distribution functions for
#' @param fun The distribution function to return.
#' @returns An R function with the name `distribution_{fun}`.
#' Will throw an error if this function does not exist
get_dist <- function(model, fun = "lcdf") {
  dist <- stringr::str_match(model$family$name, "metad__[[:digit:]]*__(.*)__[[:alpha:]]*")[, 2]
  paste0(dist, "_", fun) |>
    sym() |>
    eval()
}

#' Get the parameterization of `meta_c` in `model`
#' @param model the `brms` model
#' @returns A character vector, either `"absolute"` or `"relative"`.
get_metac <- function(model) {
  stringr::str_match(model$family$name, "metad__[[:digit:]]*__.*__([[:alpha:]]*)")[, 2]
}

#' The normal cumulative distribution function
#' @param x The value to evaluate the lcdf at
#' @param mu The mean of the normal distribution
#' @returns `log(P(X < x))` where X is sampled from a normal distribution
#' with mean `mu` and standard deviation of `1`
#' @rdname normal_dist
#' @export
normal_lcdf <- function(x, mu) pnorm(x, mean = mu, log.p = TRUE)

#' The complement of the normal cumulative distribution function
#' @param x The value to evaluate the lccdf at
#' @param mu The mean of the normal distribution
#' @returns `log(P(X > x))` where X is sampled from a normal distribution
#' with mean `mu` and standard deviation of `1`
#' @rdname normal_dist
#' @export
normal_lccdf <- function(x, mu) pnorm(x, mean = mu, log.p = TRUE, lower.tail = FALSE)

#' Generate (log) probability simplex over the joint type 1/type 2 responses
#' @param stimulus the stimulus (0 or 1)
#' @param d_prime the type 1 sensitivity
#' @param c the type 1 response criterion
#' @param meta_d_prime the type 2 sensitivity
#' @param meta_c the type 1 criteriom for generating confidence ratings
#' @param meta_c2_0 the type 2 response criteria for `"0"` responses
#' @param meta_c2_1 the type 2 response criteria for `"1"` responses
#' @param lcdf The log cumulative distribution function for the underlying distribution in the metad' model.
#' By default, uses the normal distribution with a standard deviation of `1`.
#' @param lccdf The log complement cumulative distribution function for the underlying distribution in the metad' model.
#' By default, uses the normal distribution with a standard deviation of `1`.
#' @param log if TRUE, return log probabilities instead of probabilities
#' @export
metad_pmf <- function(stimulus, d_prime, c,
                      meta_d_prime, meta_c,
                      meta_c2_0, meta_c2_1,
                      lcdf = normal_lcdf, lccdf = normal_lccdf,
                      log = FALSE) {
  # number of confidence levels
  K <- length(meta_c2_0) + 1

  # type-1 response probabilities
  lp_1 <- lccdf(c, to_signed(stimulus) * d_prime / 2)
  lp_0 <- lcdf(c, to_signed(stimulus) * d_prime / 2)

  # calculate normal cdfs (log scale)
  lp2_1 <- lccdf(c(meta_c, meta_c2_1), to_signed(stimulus) * meta_d_prime / 2)
  lp2_0 <- lcdf(c(meta_c, meta_c2_0), to_signed(stimulus) * meta_d_prime / 2)

  # response probabilities
  log_theta <- rep(0, 2 * K)
  for (k in 1:(K - 1)) {
    log_theta[K - k + 1] <- log(exp(lp2_0[k]) - exp(lp2_0[k + 1]))
    log_theta[K + k] <- log(exp(lp2_1[k]) - exp(lp2_1[k + 1]))
  }
  log_theta[1] <- lp2_0[K]
  log_theta[2 * K] <- lp2_1[K]

  # weight by P(response|stimulus) and normalize
  log_theta[1:K] <- log_theta[1:K] + lp_0 - lp2_0[1]
  log_theta[(K + 1):(2 * K)] <- log_theta[(K + 1):(2 * K)] + lp_1 - lp2_1[1]

  if (log) {
    log_theta
  } else {
    exp(log_theta)
  }
}

#' Generate posterior predictions for the metad' model
#' @param prep an object containing the data and model draws
#' @returns A `[D x N x K*4]` array containing posterior samples of
#' the joint probability of a type 1/type 2 response,
#' where `D` is the number of posterior draws,
#' `N` is the number of rows in the data, and
#' `K` is the number of confidence levels.
posterior_epred_metad <- function(prep) {
  M <- brms::get_dpar(prep, "mu")
  d_prime <- brms::get_dpar(prep, "dprime")
  c1 <- brms::get_dpar(prep, "c")

  # align dimensions
  n_obs <- dim(M)[2]
  if (is.vector(d_prime)) {
    d_prime <- replicate(n_obs, d_prime)
  }
  if (is.vector(c1)) {
    c1 <- replicate(n_obs, c1)
  }
  meta_d_prime <- M * d_prime
  meta_c <- NULL
  if (get_metac(prep) == "absolute") {
    meta_c <- c1
  } else {
    meta_c <- M * c1
  }

  # determine confidence thresholds
  dpars <- names(prep$dpars)
  meta_c2_0 <- NULL
  meta_c2_1 <- NULL

  if (is.vector(brms::get_dpar(prep, "metac2zero1diff"))) {
    if (length(dpars[stringr::str_detect(dpars, "metac2zero")]) == 1) {
      meta_c2_0 <- brms::get_dpar(prep, "metac2zero1diff")
      meta_c2_0 <- array(meta_c2_0, dim = c(length(meta_c2_0), 1, 1))
    } else {
      meta_c2_0 <- dpars[stringr::str_detect(dpars, "metac2zero")] |>
        sapply(function(s) brms::get_dpar(prep, s)) |>
        apply(1, cumsum) |>
        t() |>
        replicate(last(dim(meta_c)), expr = _) |>
        aperm(c(1, 3, 2))
    }
  } else {
    meta_c2_0 <- dpars[stringr::str_detect(dpars, "metac2zero")] |>
      lapply(function(s) brms::get_dpar(prep, s)) |>
      abind::abind(along = 3) |>
      apply(1:2, cumsum) |>
      aperm(c(2, 3, 1))
  }

  if (is.vector(brms::get_dpar(prep, "metac2one1diff"))) {
    if (length(dpars[stringr::str_detect(dpars, "metac2one")]) == 1) {
      meta_c2_1 <- brms::get_dpar(prep, "metac2one1diff")
      meta_c2_1 <- array(meta_c2_1, dim = c(length(meta_c2_1), 1, 1))
    } else {
      meta_c2_1 <- dpars[stringr::str_detect(dpars, "metac2one")] |>
        sapply(function(s) brms::get_dpar(prep, s)) |>
        apply(1, cumsum) |>
        t() |>
        replicate(last(dim(meta_c)), expr = _) |>
        aperm(c(1, 3, 2))
    }
  } else {
    meta_c2_1 <- dpars[stringr::str_detect(dpars, "metac2one")] |>
      lapply(function(s) brms::get_dpar(prep, s)) |>
      abind::abind(along = 3) |>
      apply(1:2, cumsum) |>
      aperm(c(2, 3, 1))
  }

  # calculate number of confidence thresholds
  k <- last(dim(meta_c2_0))
  K <- k + 1

  # calculate confidence threhsolds
  meta_c2_0 <- replicate(k, meta_c) - meta_c2_0
  meta_c2_1 <- replicate(k, meta_c) + meta_c2_1


  # calculate joint response & confidence probabilities
  lcdf <- get_dist(prep, fun = "lcdf")
  lccdf <- get_dist(prep, fun = "lccdf")
  p <- array(dim = c(dim(d_prime), 4 * K))
  for (s in 1:first(dim(d_prime))) {
    for (i in 1:last(dim(d_prime))) {
      p[s, i, 1:(2 * K)] <-
        metad_pmf(0, d_prime[s, i], c1[s, i], meta_d_prime[s, i],
          meta_c[s, i], meta_c2_0[s, i, ], meta_c2_1[s, i, ],
          lcdf = lcdf, lccdf = lccdf
        )
      p[s, i, (2 * K + 1):(4 * K)] <-
        metad_pmf(1, d_prime[s, i], c1[s, i], meta_d_prime[s, i],
          meta_c[s, i], meta_c2_0[s, i, ], meta_c2_1[s, i, ],
          lcdf = lcdf, lccdf = lccdf
        )
    }
  }

  p
}

#' Calculate the log probability simplex of the metad' model
#' @param i an observation index
#' @param prep an object containing the data and model draws
#' @returns A vector of joint type 1/type 2 response probabilties
#' for observation `i` in `prep`
lp_metad <- function(i, prep) {
  M <- brms::get_dpar(prep, "mu", i = i)
  d_prime <- brms::get_dpar(prep, "dprime", i = i)
  c1 <- brms::get_dpar(prep, "c", i = i)
  meta_d_prime <- M * d_prime
  meta_c <- NULL
  if (get_metac(prep) == "absolute") {
    meta_c <- c1
  } else {
    meta_c <- M * c1
  }

  # determine confidence thresholds
  dpars <- names(prep$dpars)
  meta_c2_0 <- dpars[stringr::str_detect(dpars, "metac2zero")] |>
    sapply(function(s) brms::get_dpar(prep, s, i = i))
  if (is.vector(meta_c2_0)) {
    meta_c2_0 <- matrix(meta_c2_0, ncol = length(meta_c2_0))
  }
  meta_c2_0 <- meta_c2_0 |>
    apply(1, cumsum) |>
    t()
  meta_c2_1 <- dpars[stringr::str_detect(dpars, "metac2one")] |>
    sapply(function(s) brms::get_dpar(prep, s, i = i))
  if (is.vector(meta_c2_1)) {
    meta_c2_1 <- matrix(meta_c2_1, ncol = length(meta_c2_1))
  }
  meta_c2_1 <- meta_c2_1 |>
    apply(1, cumsum) |>
    t()
  meta_c2_0 <- meta_c - meta_c2_0
  meta_c2_1 <- meta_c + meta_c2_1
  meta_c2_0 <- split(meta_c2_0, row(meta_c2_0))
  meta_c2_1 <- split(meta_c2_1, row(meta_c2_1))

  # calculate joint response & confidence probabilities
  lcdf <- get_dist(prep, fun = "lcdf")
  lccdf <- get_dist(prep, fun = "lccdf")
  PMF <- Vectorize(metad_pmf,
    vectorize.args = c(
      "stimulus", "d_prime", "c", "meta_d_prime",
      "meta_c", "meta_c2_0", "meta_c2_1"
    )
  )
  lp_0 <- PMF(0, d_prime, c1, meta_d_prime, meta_c, meta_c2_0, meta_c2_1,
    log = TRUE, lcdf = lcdf, lccdf = lccdf
  )
  lp_1 <- PMF(1, d_prime, c1, meta_d_prime, meta_c, meta_c2_0, meta_c2_1,
    log = TRUE, lcdf = lcdf, lccdf = lccdf
  )

  t(rbind(lp_0, lp_1))
}

#' Generate a function to calculate the log likelihood of the metad' model
#' @param i an observation index
#' @param prep an object containing the data and model draws
#' @returns A `[D x K*4]` array containing posterior samples of
#' the joint probability of a type 1/type 2 response,
#' where `D` is the number of posterior draws,
#' `N` is the number of rows in the data, and
#' `K` is the number of confidence levels.
log_lik_metad <- function(i, prep) {
  p <- exp(lp_metad(i, prep))

  if (any(is.na(prep$data$Y))) {
    stop("Error: please provide sample data y with trial counts")
  }

  y <- prep$data$Y[i, ]
  N_0 <- sum(y[1:(length(y) / 2)])
  N_1 <- sum(y[(length(y) / 2 + 1):length(y)])

  # calculate multinomial response probabilities
  apply(
    p[, 1:(ncol(p) / 2), drop = FALSE], 1,
    function(prob) {
      dmultinom(y[1:(length(y) / 2)],
        size = N_0, prob = prob, log = TRUE
      )
    }
  ) +
    apply(
      p[, (ncol(p) / 2 + 1):ncol(p), drop = FALSE], 1,
      function(prob) {
        dmultinom(y[(length(y) / 2 + 1):length(y)],
          size = N_1, prob = prob, log = TRUE
        )
      }
    )
}

#' Simulate posterior predictions from the metad' model
#' @param i an observation index
#' @param prep an object containing the data and model draws
#' @param ... Additional arguments. Not currently used.
#' @returns A `[D x K*4]` array containing posterior samples of
#' counts of joint type 1/type 2 responses,
#' where `D` is the number of posterior draws,
#' `N` is the number of rows in the data, and
#' `K` is the number of confidence levels.
posterior_predict_metad <- function(i, prep, ...) {
  p <- exp(lp_metad(i, prep))

  if (any(is.na(prep$data$Y))) {
    stop("Error: please provide sample data y with trial counts")
  }

  y <- prep$data$Y[i, ]
  N_0 <- as.integer(sum(y[1:(length(y) / 2)]))
  N_1 <- as.integer(sum(y[(length(y) / 2 + 1):length(y)]))

  # simulate from a multinomial distribution
  rbind(
    apply(p[, 1:(ncol(p) / 2), drop = FALSE], 1, rmultinom, n = 1, size = N_0),
    apply(p[, (ncol(p) / 2 + 1):ncol(p), drop = FALSE], 1, rmultinom, n = 1, size = N_1)
  ) |>
    t()
}

#' Generate a brms family for the metad' model with K confidence levels
#' @param K The number of confidence levels
#' @param distribution The noise distribution to use for the signal detection model
#' @param metac_absolute If `TRUE`, fix the type 2 criterion to be equal to the type 1 criterion.
#' Otherwise, equate the criteria relatively such that `metac/metadprime = c/dprime`.
#' @returns A `brms` family for the metad' model with K confidence levels
#' @export
metad <- function(K, distribution = "normal", metac_absolute = TRUE) {
  k <- K - 1
  brms::custom_family(
    name = paste0(
      "metad__", K, "__", distribution, "__",
      ifelse(metac_absolute, "absolute", "relative")
    ),
    dpars = c(
      "mu", "dprime", "c", paste0("metac2zero", 1:k, "diff"),
      paste0("metac2one", 1:k, "diff")
    ),
    links = c("log", "identity", "identity", rep("log", 2 * k)),
    lb = c(0, NA, NA, rep(0, 2 * k)),
    type = "int", specials = c("multinomial"),
    log_lik = log_lik_metad,
    posterior_predict = posterior_predict_metad,
    posterior_epred = posterior_epred_metad
  )
}
