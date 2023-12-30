#' @title 
#' Assessing bias
#' 
#' @description
#' Assessment of absolute bias in posterior means and medians for a given weight and evidence level, using simulated data as input.
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
#' @return A 2-dimensional array containing results on bias.
#' @export
#' 
#' @seealso [oc_pos()] and [oc_coverage()].
#'
#' @examples
#' set.seed(123)
#' n_sims <- 5 # small number for exemplary application 
#' sim_dat <- list(
#'   "m" = rnorm(n = n_sims, mean = 1.15, sd = 0.1),
#'   "se" = rnorm(n = n_sims, mean = 1.8, sd = 0.3)
#' )
#' results <- oc_bias(
#'   m = sim_dat[["m"]],
#'   se = sim_dat[["se"]],
#'   true_effect = 1.15,
#'   weights = seq(0, 1, by = 0.01), 
#'   map_prior = load_tipmap_data("tipmapPrior.rds"), 
#'   sigma = 16.23,
#'   eval_strategy = "sequential",
#'   n_cores = 1
#' ) 
#' print(results)
oc_bias <- function(
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
  # vector of types of bias 
  oc_types <- c("abs.bias.post.mean", "abs.bias.post.median")
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
      x[i, 1] <- summary(posterior)["mean"] - true_effect
      old_w <- getOption("warn"); options(warn = -1)
       q.post <- RBesT::qmix(mix = posterior, p = 0.5)
       names(q.post) <- c("p0.5")
       x[i, 2] <- q.post["p0.5"] - true_effect
      options(warn = old_w)
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