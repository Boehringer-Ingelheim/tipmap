#' @title 
#' Assessing probability of success 
#' 
#' @description
#' Assessment of the probability of truly or falsely (depending on simulated scenario) rejecting the null hypothesis of interest for a given weight and evidence level, using simulated data as input.
#'
#' @param m Numerical vector of simulated effect estimates. 
#' @param se Numerical vector of simulated standard errors (\code{m} and \code{se} need to have the same length). 
#' @param probs Vector of quantiles q, with 1 minus q representing an evidence level of interest (where positive effect estimate indicate a beneficial treatment).
#' @param weights Vector of weights of the informative component of the MAP prior (defaults to \code{seq(0, 1, by = 0.01)}).
#' @param map_prior A MAP prior containing information about the trials in the source population, created using \code{RBesT}; a mixture of normal distributions is required.
#' @param sigma Standard deviation of the weakly informative component of the MAP prior, recommended to be the unit-information standard deviation. 
#' @param null_effect Numerical value, representing the null effect (defaults to 0).
#' @param direction_pos Logical value, \code{TRUE} (default) if effects greater that the \code{null_effect} indicate a beneficial treatment and \code{FALSE} otherwise.
#' @param eval_strategy Character variable, representing the evaluation strategy, either "sequential", "multisession", or "multicore" (see documentation of \code{future::plan}, defaults to "sequential").
#' @param n_cores Integer value, representing the number of cores to be used (defaults to 1); only applies if \code{eval_strategy} is not "sequential".
#' 
#' @return A 2-dimensional array containing probabilities, either of truly (probability of success) or falsely rejecting the null hypothesis of interest for a given weight and evidence level.
#' 
#' @export
#' 
#' @seealso [oc_bias()] and [oc_coverage()].
#' 
#' @examples
#' set.seed(123)
#' n_sims <- 5 # small number for exemplary application
#' sim_dat <- list(
#'   "m" = rnorm(n = n_sims, mean = 1.15, sd = 0.1),
#'   "se" = rnorm(n = n_sims, mean = 1.8, sd = 0.3)
#' )
#' results <- oc_pos(
#'   m = sim_dat[["m"]],
#'   se = sim_dat[["se"]],
#'   probs = c(0.025, 0.05, 0.1, 0.2), 
#'   weights = seq(0, 1, by = 0.01), 
#'   map_prior = load_tipmap_data("tipmapPrior.rds"), 
#'   sigma = 16.23,
#'   null_effect = 0,
#'   direction_pos = TRUE, 
#'   eval_strategy = "sequential",
#'   n_cores = 1
#' ) 
#' print(results)
oc_pos <- function(
    m, se, probs, weights = seq(0, 1, by = 0.01),
    map_prior, sigma,
    null_effect = 0, direction_pos = T,
    n_cores = 1, eval_strategy = "sequential"
    ) {
  # check inputs
  assert_that(is.numeric(m))
  assert_that(is.numeric(se))
  assert_that(all(se > 0))
  assert_that(length(m) == length(se))
  assert_that(is.numeric(probs))
  assert_that(all(probs > 0))
  assert_that(all(probs < 1))
  assert_that(is.numeric(weights))
  assert_that(all(weights >= 0))
  assert_that(all(weights <= 1))
  assert_that("normMix" %in% class(map_prior))
  assert_that(is.numeric(sigma))
  assert_that(is.scalar(sigma))
  assert_that(is.numeric(null_effect))
  assert_that(is.flag(direction_pos))
  assert_that(eval_strategy %in% c("sequential", "multisession", "multicore"))
  assert_that(is.count(n_cores))
  # function to compute posterior quantiles
  # for one trial
  oc_post_q <- function(
    est, se, probs, weights, map_prior, sigma, 
    null_effect, direction_pos
  ) {
    x <- array(dim = c(length(weights), length(probs)))
    dimnames(x) <- list(paste("w=", weights, sep = ""),
                        paste("q=", probs, sep = ""))
    for (i in 1:length(weights)) {
      w <- weights[i]
      robust.mix.prior <- RBesT::robustify(
        prior = map_prior,
        weight = (1 - w),
        m = 0,
        n = 1,
        sigma = sigma
      )
      posterior <- RBesT::postmix(
        robust.mix.prior,
        m = est,
        se = se
      )
      ifelse(
        test = (direction_pos == T), 
        yes = x[i,] <- RBesT::qmix(posterior, p = probs) > null_effect, 
        no = x[i,] <- RBesT::qmix(posterior, p = probs) < null_effect
      )
    }
    return(x)
  }
  # perform pos calculation 
  ifelse(
    test = (eval_strategy == "sequential"),
    yes = future::plan(strategy = eval_strategy), 
    no = future::plan(strategy = eval_strategy, workers = n_cores)
  )
  results <- furrr::future_map2(
    .x = m,
    .y = se,
    .f = ~ oc_post_q(
      est = .x,
      se = .y,
      probs = probs,
      weights = weights,
      map_prior = map_prior,
      sigma = sigma,
      null_effect = null_effect,
      direction_pos = direction_pos
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
