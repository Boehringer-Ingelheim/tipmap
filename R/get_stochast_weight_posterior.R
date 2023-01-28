#' Computation of posterior distribution using weights sampled from a distribution of weights
#'
#' @description
#' Performs repeated analyses with weights on the informative component of the MAP prior randomly drawn from a distribution of weights and combines samples from all posteriors.
#'
#' @param map_prior The MAP prior to be robustified, created using \code{RBesT::automixfit()}.
#' @param new_trial_dat A vector summarising the new trial data. See \code{createNewTrialData()}.
#' @param weights A vector containing the weights to be assigned to the informative component.
#' @param n_posterior_samples The number of samples to be drawn from each posterior.
#' @param null_effect The null treatment effect. Defaults to 0.
#' @param sigma Unit information standard deviation used by \code{RBesT::robustify()}.
#'
#' @return Sample of the posterior distribution.
#'
#' @seealso \code{\link{create_new_trial_data}}, \code{\link{draw_beta_mixture_nsamples}}.
#'
#' @examples
#'
#' # MAP prior
#' (map_prior <- load_tipmap_data("tipmapPrior.rds"))
#' summary(map_prior)
#'
#' # New trial data in target population
#' new_trial_dat <- create_new_trial_data(
#'   n_total = 30, est = 1.17, se = 0.95
#' )
#'
#' # Expert weights
#' expert_weights <- rbind(
#'   c(0, 0, 0, 0, 1, 3, 3, 2, 1, 0),
#'   c(0, 0, 1, 1, 2, 2, 3, 1, 0, 0),
#'   c(0, 0, 0, 0, 2, 3, 2, 2, 1, 0)
#' )
#' set.seed(123)
#' sampled_weights <- draw_beta_mixture_nsamples(expert_weights, n = 500)
#' (m_w <- round(mean(sampled_weights), 2)) # 0.59
#'
#' # Posterior based on mean weight
#' robust_mix_prior <- RBesT::robustify(
#'   priormix = map_prior,
#'   weight = (1 - m_w),
#'   n = 1,
#'   mean = 0,
#'   sigma = 12
#' )
#' posterior1 <- RBesT::postmix(
#'   priormix = robust_mix_prior,
#'   m = new_trial_dat["mean"],
#'   se = new_trial_dat["se"]
#' )
#'
#' # Posterior based on a sample of weights
#' posterior2 <- get_stochast_weight_posterior(
#'   map_prior = map_prior,
#'   new_trial_dat = new_trial_dat,
#'   weights = sampled_weights,
#'   n_posterior_samples = 500,
#'   sigma = 12
#' )
#' summary_posterior2 <- c(mean(posterior2), sd(posterior2),
#'                         quantile(posterior2, probs = c(0.025, 0.5, 0.975)))
#' names(summary_posterior2) <- c("mean", "sd", "2.5%", "50.0%", "97.5%")
#'
#' # Comparison of posterior 1 and 2
#' summary(posterior1)
#' print(summary_posterior2)
#'
get_stochast_weight_posterior <- function(
    map_prior,
    new_trial_dat,
    weights,
    n_posterior_samples,
    null_effect = 0,
    sigma) {
  # checks
  if (!(is.numeric(weights))) stop("weights must be a numeric vector")
  if (!(is.numeric(n_posterior_samples))) stop("n_posterior_samples must be a numeric value")
  if (!(is.numeric(null_effect))) stop("null_effect must be a numeric value")
  if (!(is.numeric(sigma))) stop("sigma must be a numeric value")
  # computing stochastic weight posterior
  arr <- array(dim = c(length(weights), n_posterior_samples))
  for (i in 1:length(weights)) {
    robust_mix_prior <- RBesT::robustify(
      priormix = map_prior,
      weight = (1 - weights[i]),
      n = 1,
      mean = null_effect,
      sigma = sigma)
    posterior <- RBesT::postmix(
      priormix = robust_mix_prior,
      m = new_trial_dat["mean"],
      se = new_trial_dat["se"])
    arr[i, ] <- RBesT::rmix(posterior, n=n_posterior_samples)
  }
  posterior_dat <- c(arr)
  return(posterior_dat)
}
