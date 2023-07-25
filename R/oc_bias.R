#' @title 
#' Assessing bias
#' 
#' @description
#' Assessment of absolute bias in posterior means and medians for a given weight and evidence level, using simulated data as input.
#'
#' @param m Numerical vector of simulated effect estimates.
#' @param se Numerical vector of simulated standard errors (`m` and `se` need to have the same length). 
#' @param true_effect Numerical value, representing the true treatment effect (usually the mean of the simulated `m`).
#' @param weights Vector of weights of the informative component of the MAP prior.
#' @param map_prior A MAP prior containing information about the trial(s) in the source population, created using \code{RBesT}.
#' @param sigma Standard deviation to be used for the weakly informative component of the MAP prior, recommended to be the unit-information standard deviation. 
#'
#' @return A 2-dimensional array containing data on bias.
#' @export
#' 
#' @seealso \code{\link{oc_pos}} and \code{\link{oc_coverage}}
#'
#' @examples
#' set.seed(123)
#' n_sims <- 10 # small number for exemplary application 
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
#'   sigma = 16.23
#' ) 
#' print(results)
oc_bias <- function(
    m, se, true_effect, weights, map_prior, sigma
  ) {
  # check inputs
  assert_that(is.numeric(m))
  assert_that(is.numeric(se))
  assert_that(length(m) == length(se))
  assert_that(is.numeric(true_effect))
  assert_that(is.scalar(true_effect))
  assert_that(is.numeric(weights))
  assert_that(all(weights >= 0))
  assert_that(all(weights <= 1))
  assert_that("normMix" %in% class(map_prior))
  assert_that(is.numeric(sigma))
  assert_that(is.scalar(sigma))
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
      q.post <- suppressWarnings(RBesT::qmix(mix = posterior, p = 0.5))
      names(q.post) <- c("p0.5")
      x[i, 1] <- summary(posterior)["mean"] - true_effect
      x[i, 2] <- q.post["p0.5"] - true_effect
    }
  return(x)
  }
  # compute bias over a range of (simulated) trial results
  results <- purrr::map2(
    .x = m,
    .y = se,
    .f = ~ assess_one_trial(
      m = .x,
      se = .y,
      true_effect = true_effect,
      weights = weights,
      map_prior = map_prior,
      sigma = sigma
      )
    ) %>%
    simplify2array() %>%
    apply(c(1, 2), mean)
  return(results)
}