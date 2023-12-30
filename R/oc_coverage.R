#' @title 
#' Assessing coverage
#' 
#' @description
#' Assessment of coverage of posterior intervals for a given weight and evidence level, using simulated data as input.
#'
#' @param m Numerical vector of simulated effect estimates.
#' @param se Numerical vector of simulated standard errors (\code{m} and \code{se} need to have the same length). 
#' @param true_effect Numerical value, representing the true treatment effect (usually the mean of the simulated \code{m}).
#' @param weights Vector of weights of the informative component of the MAP prior (defaults to \code{seq(0, 1, by = 0.01)}).
#' @param map_prior A MAP prior containing information about the trials in the source population, created using \code{RBesT}; a mixture of normal distributions is required.
#' @param sigma Standard deviation of the weakly informative component of the MAP prior, recommended to be the unit-information standard deviation. 
#' @param eval_strategy Character variable, representing the evaluation strategy, either "sequential", "multisession", or "multicore" (see documentation of \code{future::plan}, defaults to "sequential").
#' @param n_cores Integer value, representing the number of cores to be used (defaults to 1); only applies if \code{eval_strategy} is not "sequential".
#' 
#' @return A 2-dimensional array containing results on coverage.
#' @export
#' 
#' @seealso [oc_pos()] and [oc_bias()].
#'
#' @examples
#' set.seed(123)
#' n_sims <- 5 # small number for exemplary application 
#' sim_dat <- list(
#'   "m" = rnorm(n = n_sims, mean = 1.15, sd = 0.1),
#'   "se" = rnorm(n = n_sims, mean = 1.8, sd = 0.3)
#' )
#' results <- oc_coverage(
#'   m = sim_dat[["m"]],
#'   se = sim_dat[["se"]],
#'   true_effect = 1.15,
#'   weights = seq(0, 1, by = 0.01), 
#'   map_prior = load_tipmap_data("tipmapPrior.rds"), 
#'   sigma = 16.23
#' ) 
#' print(results)
oc_coverage <- function(
    m, se, true_effect, weights = seq(0, 1, by = 0.01),
    map_prior, sigma,
    n_cores = 1, eval_strategy = "sequential"
  ) {
  # check inputs
  assert_that(is.numeric(m))
  assert_that(is.numeric(se))
  assert_that(all(se > 0))
  assert_that(length(m) == length(se))
  assert_that(is.numeric(true_effect))
  assert_that(is.scalar(true_effect))
  assert_that(is.numeric(weights))
  assert_that(all(weights >= 0))
  assert_that(all(weights <= 1))
  assert_that("normMix" %in% class(map_prior))
  assert_that(is.numeric(sigma))
  assert_that(is.scalar(sigma))
  assert_that(sigma > 0)
  assert_that(eval_strategy %in% c("sequential", "multisession", "multicore"))
  assert_that(is.count(n_cores))
  # vector of types of coverage probabilities
  oc_types <- c("cov.50p", "cov.80p", "cov.90p", "cov.95p")
  # vector of probabilities
  probs <- c(0.025, 0.05, 0.1, 0.25, 0.75, 0.9, 0.95, 0.975)
  # function to compare posterior to truth for one trial
  assess_one_trial <- function(
    m, se, true_effect, weights, map_prior, sigma
    ) {
    # create 2-dimensional array
    x <- array(dim = c(length(weights), length(oc_types)))
    dimnames(x) <- list(
      "weights" = paste("w=", weights, sep = ""),
      "oc_types" = oc_types
    )
    # loop over weights
    for (i in 1:length(weights)) {
      w <- weights[i]
      robust.mix.prior <- RBesT::robustify(
        prior = map_prior,
        weight = (1 - w),
        m = 0,
        n = 1,
        sigma = sigma
      )
      posterior <- RBesT::postmix(robust.mix.prior, m = m, se = se)
      q.post <- RBesT::qmix(mix = posterior, p = probs)
      names(q.post) <- c(
        "p0.025", "p0.05", "p0.1",  "p0.25", 
        "p0.75", "p0.9",  "p0.95", "p0.975"
      )
      x[i, 1] <- dplyr::between(true_effect, q.post["p0.25"], q.post["p0.75"])
      x[i, 2] <- dplyr::between(true_effect, q.post["p0.1"], q.post["p0.9"])
      x[i, 3] <- dplyr::between(true_effect, q.post["p0.05"], q.post["p0.95"])
      x[i, 4] <- dplyr::between(true_effect, q.post["p0.025"], q.post["p0.975"])
    }
  return(x)
  }
  # compute bias over a range of (simulated) trial results
  ifelse(
    test = (eval_strategy == "sequential"),
    yes = future::plan(strategy = eval_strategy), 
    no = future::plan(strategy = eval_strategy, workers = n_cores)
  )
  results <- furrr::future_map2(
    .x = m,
    .y = se,
    .f = ~ assess_one_trial(
      m = .x,
      se = .y,
      true_effect = true_effect,
      weights = weights,
      map_prior = map_prior,
      sigma = sigma
      ),
    .options = furrr::furrr_options(),
    .env_globals = parent.frame(),
    .progress = F
    ) %>%
    simplify2array() %>%
    apply(c(1, 2), mean)
  if (!(eval_strategy == "sequential")) future::plan(strategy = "sequential")
  return(results)
}