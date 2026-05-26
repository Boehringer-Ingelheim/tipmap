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
#' Highest posterior density intervals are based on `coda::HPDinterval()` and
#' are an experimental feature.
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
create_posterior_data <- function(
    map_prior,
    new_trial_data,
    sigma,
    null_effect = 0,
    interval_type = "equal-tailed",
    n_samples = 1e4
) {
  required_new_trial_names <- c("mean", "se")
  
  assert_that(
    "normMix" %in% class(map_prior),
    msg = "`map_prior` must be a normal mixture prior, e.g. from RBesT"
  )
  
  assert_that(is.numeric(new_trial_data), msg = "`new_trial_data` must be numeric")
  assert_that(!is.null(names(new_trial_data)), msg = "`new_trial_data` must be a named numeric vector")
  assert_that(
    all(required_new_trial_names %in% names(new_trial_data)),
    msg = "`new_trial_data` must contain at least `mean` and `se`"
  )
  assert_that(all(is.finite(new_trial_data[c("mean", "se")])), msg = "`new_trial_data` values must be finite")
  assert_that(new_trial_data["se"] > 0, msg = "`new_trial_data['se']` must be positive")
  
  assert_that(is.numeric(sigma), msg = "`sigma` must be numeric")
  assert_that(length(sigma) == 1, msg = "`sigma` must be length 1")
  assert_that(is.finite(sigma), msg = "`sigma` must be finite")
  assert_that(sigma > 0, msg = "`sigma` must be positive")
  
  assert_that(is.numeric(null_effect), msg = "`null_effect` must be numeric")
  assert_that(length(null_effect) == 1, msg = "`null_effect` must be length 1")
  assert_that(is.finite(null_effect), msg = "`null_effect` must be finite")
  
  assert_that(
    is.character(interval_type) && length(interval_type) == 1,
    msg = "`interval_type` must be a character scalar"
  )
  assert_that(
    interval_type %in% c("equal-tailed", "hpdi"),
    msg = "`interval_type` must be either 'equal-tailed' or 'hpdi'"
  )
  
  if (identical(interval_type, "hpdi")) {
    assert_that(is.numeric(n_samples), msg = "`n_samples` must be numeric")
    assert_that(length(n_samples) == 1, msg = "`n_samples` must be length 1")
    assert_that(is.finite(n_samples), msg = "`n_samples` must be finite")
    assert_that(n_samples == round(n_samples), msg = "`n_samples` must be a whole number")
    assert_that(n_samples > 0, msg = "`n_samples` must be positive")
  }
  
  arr <- array(dim = c(length(default_weights), length(default_quantiles)))
  dimnames(arr) <- list(
    paste0("w=", default_weights),
    paste0("q", default_quantiles)
  )
  
  make_posterior <- function(w) {
    robust.mix.prior <- RBesT::robustify(
      map_prior,
      weight = 1 - w,
      m = 0,
      n = 1,
      sigma = sigma,
      mean = null_effect
    )
    
    RBesT::postmix(
      robust.mix.prior,
      m = unname(new_trial_data["mean"]),
      se = unname(new_trial_data["se"])
    )
  }
  
  if (identical(interval_type, "equal-tailed")) {
    for (i in seq_along(default_weights)) {
      posterior <- make_posterior(default_weights[i])
      suppressWarnings(arr[i, ] <- RBesT::qmix(posterior, default_quantiles))
    }
  }
  
  if (identical(interval_type, "hpdi")) {
    lower_probs <- default_quantiles[default_quantiles < 0.5]
    median_idx <- which(default_quantiles == 0.5)
    
    for (i in seq_along(default_weights)) {
      posterior <- make_posterior(default_weights[i])
      mix_norm_samples <- RBesT::rmix(mix = posterior, n = n_samples) |> coda::mcmc()
      
      hpdi_bounds <- numeric(length(default_quantiles))
      for (j in seq_along(lower_probs)) {
        p <- lower_probs[j]
        bounds <- coda::HPDinterval(mix_norm_samples, prob = 1 - 2 * p)[1, ]
        hpdi_bounds[j] <- bounds[1]
        hpdi_bounds[length(default_quantiles) - j + 1] <- bounds[2]
      }
      hpdi_bounds[median_idx] <- RBesT::qmix(posterior, p = 0.5)
      arr[i, ] <- hpdi_bounds
    }
  }
  
  data.frame(weight = default_weights, arr, check.names = FALSE)
}