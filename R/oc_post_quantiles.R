#' @title 
#' Quantiles of posterior for a range of weights
#'
#' @description
#' Computes quantiles of the posterior for a range of weights and checks (for one trial) whether these quantiles are larger or lower, respectively, than a defined null effect; it is used to calculate the probability of truly (probability of success) or falsely rejecting the null hypothesis of interest for a given weight and evidence level.
#'
#' @param est Treatment effect estimate.
#' @param se Standard error of the treatment effect estimate.
#' @param probs Vector of quantiles q, with 1 minus q representing an evidence level of interest (where positive effect estimate indicate a beneficial treatment).
#' @param weights Vector of weights of the informative component of the MAP prior.
#' @param map_prior A MAP prior containing information about the trial(s) in the source population, created using \code{RBesT}.
#' @param sigma Standard deviation to be used for the weakly informative component of the MAP prior, recommended to be the unit-information standard deviation.
#' @param null_effect Numerical value representing the null effect (defaults to 0).
#' @param direction_pos Logical value, `TRUE` (default) if effects greater that the `null_effect` indicate a beneficial treatment and `FALSE` otherwise.
#'
#' @return An array of logical values indicating whether a posterior quantile is larger than the null effect (if `direction_pos == TRUE`), or smaller (if `direction_pos == FALSE`), respectively.
#'
#' @seealso [oc_pos()]
#'
#' @examples
#' oc_post_quantiles(
#'   est = 1.15,
#'   se = 1.8, 
#'   probs = c(0.975, 0.95), #c(0.025, 0.05, 0.1, 0.2), 
#'   weights = seq(0, 1, by = 0.01), 
#'   map_prior = load_tipmap_data("tipmapPrior.rds"), 
#'   sigma = 16.23,
#'   null_effect = 0,
#'   direction_pos = TRUE
#' ) 
oc_post_quantiles <- function(
    est, se, probs, weights, map_prior, sigma, 
    null_effect = 0, direction_pos = TRUE
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
      test = (direction_pos == TRUE), 
      yes = x[i,] <- RBesT::qmix(posterior, p = probs) > null_effect, 
      no = x[i,] <- RBesT::qmix(posterior, p = probs) < null_effect
    )
  }
  return(x)
}
