#' @title Probability of success calculation
#' 
#' @description
#' Calculate the probability of truly (probability of success) or falsely rejecting the null hypothesis of interest for a given weight and evidence level.
#'
#' @param m Numerical vector of simulated effect estimates.
#' @param se Numerical vector of simulated standard errors. 
#' @param probs Vector of quantiles q, with 1 minus q representing an evidence level of interest (where positive effect estimate indicate a beneficial treatment).
#' @param weights Vector of weights of the informative component of the MAP prior.
#' @param map_prior A MAP prior containing information about the trial(s) in the source population, created using \code{RBesT}.
#' @param sigma Standard deviation to be used for the weakly informative component of the MAP prior, recommended to be the unit-information standard deviation. 
#' @param null_effect Numerical value representing the null effect (defaults to 0).
#' @param direction_pos Logical value, `TRUE` (default) if effects greater that the `null_effect` indicate a beneficial treatment and `FALSE` otherwise.
#'
#' @return An array of probabilities, either of truly (probability of success) or falsely rejecting the null hypothesis of interest for a given weight and evidence level.
#' 
#' @seealso [oc_post_quantiles()]
#'
#' @examples
#' set.seed(123)
#' n_sims <- 5 # small number for exemplary application
#' sim_dat <- list(
#'   "m" = rnorm(n = n_sims, mean = 1.15, sd = 0.1),
#'   "se" = rnorm(n = n_sims, mean = 1.8, sd = 0.3)
#' )
#' pos_dat <- oc_pos(
#'   m = sim_dat[["m"]],
#'   se = sim_dat[["se"]],
#'   probs = c(0.025, 0.05, 0.1, 0.2), 
#'   weights = seq(0, 1, by = 0.01), 
#'   map_prior = load_tipmap_data("tipmapPrior.rds"), 
#'   sigma = 16.23,
#'   null_effect = 0,
#'   direction_pos = TRUE
#' ) 
#' print(pos_dat)
oc_pos <- function(
    m, se, probs, weights, map_prior, sigma,
    null_effect = 0, direction_pos = TRUE
    ) {
  results <- purrr::map2(
    .x = m,
    .y = se,
    .f = ~ oc_post_quantiles(
      est = .x,
      se = .y,
      probs = probs,
      weights = weights,
      map_prior = map_prior,
      sigma = sigma,
      null_effect = null_effect,
      direction_pos = direction_pos
      )
    ) %>%
    simplify2array() %>%
    apply(c(1, 2), mean)
}
