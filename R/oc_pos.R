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
    null_effect = 0, direction_pos = TRUE,
    n_cores = 1, eval_strategy = "sequential"
) {
  assert_that(is.numeric(m), msg = "`m` must be numeric")
  assert_that(is.numeric(se), msg = "`se` must be numeric")
  assert_that(length(m) == length(se), msg = "`m` and `se` must have the same length")
  assert_that(all(is.finite(m)), msg = "`m` must be finite")
  assert_that(all(is.finite(se)), msg = "`se` must be finite")
  assert_that(all(se > 0), msg = "`se` must be positive")
  
  assert_that(is.numeric(probs), msg = "`probs` must be numeric")
  assert_that(all(is.finite(probs)), msg = "`probs` must be finite")
  assert_that(all(probs > 0 & probs < 1), msg = "`probs` must lie strictly between 0 and 1")
  
  assert_that(is.numeric(weights), msg = "`weights` must be numeric")
  assert_that(all(is.finite(weights)), msg = "`weights` must be finite")
  assert_that(all(weights >= 0 & weights <= 1), msg = "`weights` must lie in [0, 1]")
  
  assert_that("normMix" %in% class(map_prior), msg = "`map_prior` must be a normal mixture prior")
  
  assert_that(is.numeric(sigma), msg = "`sigma` must be numeric")
  assert_that(is.scalar(sigma), msg = "`sigma` must be scalar")
  assert_that(is.finite(sigma), msg = "`sigma` must be finite")
  assert_that(sigma > 0, msg = "`sigma` must be positive")
  
  assert_that(is.numeric(null_effect), msg = "`null_effect` must be numeric")
  assert_that(is.scalar(null_effect), msg = "`null_effect` must be scalar")
  assert_that(is.finite(null_effect), msg = "`null_effect` must be finite")
  
  assert_that(is.logical(direction_pos), msg = "`direction_pos` must be logical")
  assert_that(length(direction_pos) == 1, msg = "`direction_pos` must have length 1")
  assert_that(!is.na(direction_pos), msg = "`direction_pos` must be TRUE or FALSE")
  
  assert_that(
    eval_strategy %in% c("sequential", "multisession", "multicore"),
    msg = "Invalid `eval_strategy`"
  )
  assert_that(is.count(n_cores), msg = "`n_cores` must be a positive whole number")
  
  oc_post_q <- function(est, se, probs, weights, map_prior, sigma, null_effect, direction_pos) {
    x <- array(dim = c(length(weights), length(probs)))
    dimnames(x) <- list(paste0("w=", weights), paste0("q=", probs))
    
    for (i in seq_along(weights)) {
      robust.mix.prior <- RBesT::robustify(
        prior = map_prior,
        weight = 1 - weights[i],
        m = 0,
        n = 1,
        sigma = sigma
      )
      
      posterior <- RBesT::postmix(robust.mix.prior, m = est, se = se)
      qmix_vals <- RBesT::qmix(posterior, p = probs)
      
      if (direction_pos) {
        x[i, ] <- qmix_vals > null_effect
      } else {
        x[i, ] <- qmix_vals < null_effect
      }
    }
    
    x
  }
  
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  
  if (identical(eval_strategy, "sequential")) {
    future::plan(strategy = eval_strategy)
  } else {
    future::plan(strategy = eval_strategy, workers = n_cores)
  }
  
  furrr::future_map2(
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
    .progress = FALSE
  ) %>%
    simplify2array() %>%
    apply(c(1, 2), mean)
}