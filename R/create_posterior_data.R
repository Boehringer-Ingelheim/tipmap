#' @title Quantiles of posterior distributions for a range of weights on the 
#' informative component of the robust MAP prior
#'
#' @description
#' Returns a data frame containing the default quantiles of posterior mixture 
#' distributions or bounds of highest posterior density intervals,
#' generated with varying weights on the informative component of the MAP 
#' prior.
#'
#' @param map_prior A MAP prior containing information about the trial(s) in 
#' the source population, created using \code{RBesT}.
#' @param new_trial_data A vector containing information about the new trial. 
#' See \code{create_new_trial_data()}.
#' @param sigma Standard deviation to be used for the weakly informative 
#' component of the MAP prior, recommended to be the unit-information standard 
#' deviation.
#' @param null_effect The mean of the robust component of the MAP prior. 
#' Defaults to 0.
#' @param interval_type The type of credible interval (character of length 1), 
#' either `equal-tailed` (default) or `hpdi`, the highest posterior density 
#' interval.
#' @param n_samples Number of samples to compute highest posterior density 
#' intervals (hence, only applicable if the `interval_type` is `hpdi`).
#'
#' @return A data frame containing the default quantiles of posterior mixture 
#' distributions or bounds of highest posterior density intervals.
#' 
#' @details
#' Highest posterior density intervals are based on `coda::HPDinterval()`.
#' 
#' @export
#' 
#' @seealso [create_new_trial_data()] and [create_prior_data()]
#' 
#' @examples
#'
#' # create vector containing data on new trial
#' new_trial_data <- create_new_trial_data(
#'   n_total = 30,
#'   est = 1.27,
#'   se = 0.95
#' )
#'
#' # read MAP prior created by RBesT
#' map_prior <- load_tipmap_data("tipmapPrior.rds")
#'
#' # create posterior data - with interval_type = equal_tailed
#' # (the default for tipping point plots)
#' posterior_data1 <- create_posterior_data(
#'   map_prior = map_prior,
#'   new_trial_data = new_trial_data,
#'   sigma = 12,
#'   interval_type = "equal-tailed"   
#' )
#' 
#' \donttest{
#' # create posterior data - with interval_type = hpdi
#' posterior_data2 <- create_posterior_data(
#'   map_prior = map_prior,
#'   new_trial_data = new_trial_data,
#'   sigma = 12,
#'   interval_type = "hpdi",
#'   n_samples = 1e4
#' )
#' }
#' 
#' @references Best, N., Price, R. G., Pouliquen, I. J., & Keene, O. N. (2021).
#' Assessing efficacy in important subgroups in confirmatory trials: An example
#' using Bayesian dynamic borrowing. Pharm Stat, 20(3), 551–562.
#' https://doi.org/10.1002/pst.2093
#'
create_posterior_data <-
  function(map_prior,
           new_trial_data,
           sigma,
           null_effect = 0,
           interval_type = "equal-tailed",
           n_samples = 1e4
  ) {
    if (!(is.numeric(sigma))) stop("sigma must be numeric")
    if ((sigma <= 0)) stop("sigma must be positive")
    assert_that(interval_type %in% c("equal-tailed", "hpdi"))
    
    arr <- array(dim = c(length(default_weights), length(default_quantiles)))
    dimnames(arr) <- list(paste0("w=", default_weights),
                          paste0("q", default_quantiles))
    
    # For equal-tailed credible intervals
    if (interval_type == "equal-tailed") {
      for (i in 1:length(default_weights)) {
        robust.mix.prior <- RBesT::robustify(
          map_prior,
          weight = (1 - default_weights[i]),
          m = 0,
          n = 1,
          sigma = sigma,
          mean = null_effect
        )
        posterior <- RBesT::postmix(robust.mix.prior,
                                    m = new_trial_data["mean"],
                                    se = new_trial_data["se"])
        suppressWarnings(arr[i, ] <- RBesT::qmix(posterior, default_quantiles))
      }
    }
    
    # For highest posterior density intervals
    if (interval_type == "hpdi") {
      for (i in 1:length(default_weights)) {
        robust.mix.prior <- RBesT::robustify(
          map_prior,
          weight = (1 - default_weights[i]),
          m = 0,
          n = 1,
          sigma = sigma,
          mean = null_effect
        )
        posterior <- RBesT::postmix(robust.mix.prior,
                                    m = new_trial_data["mean"],
                                    se = new_trial_data["se"])
        # Using posterior samples to compute the hdpi
        mix_norm_samples <- RBesT::rmix(mix = posterior, n = n_samples) |>
          coda::mcmc()
        hpdi_bounds <- numeric(length(default_quantiles))
        hpdi_bounds[c(1, 13)] <- coda::HPDinterval(
          mix_norm_samples, prob = 1-(2*default_quantiles[1]))[1:2]
        hpdi_bounds[c(2, 12)] <- coda::HPDinterval(
          mix_norm_samples, prob = 1-(2*default_quantiles[2]))[1:2]
        hpdi_bounds[c(3, 11)] <- coda::HPDinterval(
          mix_norm_samples, prob = 1-(2*default_quantiles[3]))[1:2]
        hpdi_bounds[c(4, 10)] <- coda::HPDinterval(
          mix_norm_samples, prob = 1-(2*default_quantiles[4]))[1:2]
        hpdi_bounds[c(5, 9)] <- coda::HPDinterval(
          mix_norm_samples, prob = 1-(2*default_quantiles[5]))[1:2]
        hpdi_bounds[c(6, 8)] <- coda::HPDinterval(
          mix_norm_samples, prob = 1-(2*default_quantiles[6]))[1:2]
        hpdi_bounds[7] <- RBesT::qmix(posterior, p = 0.5)
        arr[i, ] <- hpdi_bounds
      }
    }
    
    posterior_data <- data.frame(cbind(weight = default_weights, arr))
    return(posterior_data)
  }
