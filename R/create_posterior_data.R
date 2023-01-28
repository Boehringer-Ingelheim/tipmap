#' Creates posterior distributions for a range of weights on the informative component of the robust MAP prior
#'
#' @description
#' Returns a data frame containing the default quantiles of posterior mixture distributions
#' generated with varying weights on the informative component of the MAP prior.
#'
#' @param map_prior A MAP prior containing information about the trial(s) in the source population, created using \code{RBesT}.
#' @param new_trial_data A vector containing information about the new trial. See \code{create_new_trial_data()}.
#' @param sigma Standard deviation to be used for the weakly informative component of the MAP prior, recommended to be the unit-information standard deviation.
#' @param null_effect The mean of the robust component of the MAP prior. Defaults to 0.
#'
#' @return A data frame containing posterior distributions for varying weights
#' @export
#' @seealso \code{\link{create_new_trial_data}}, \code{\link{create_prior_data}}
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
#' # create posterior data
#' \dontrun{
#' posterior_data <- create_posterior_data(
#'   map_prior = map_prior,
#'   new_trial_data = new_trial_data,
#'   sigma = 12
#' )
#' }
#' @references Best, N., Price, R. G., Pouliquen, I. J., & Keene, O. N. (2021).
#' Assessing efficacy in important subgroups in confirmatory trials: An example
#' using Bayesian dynamic borrowing. Pharm Stat, 20(3), 551–562.
#' https://doi.org/10.1002/pst.2093
#'
create_posterior_data <-
  function(map_prior,
           new_trial_data,
           sigma,
           null_effect = 0) {
    if (!(is.numeric(sigma))) stop("sigma must be numeric")
    if ((sigma <= 0)) stop("sigma must be positive")

    arr <- array(dim = c(length(default_weights), length(default_quantiles)))
    dimnames(arr) <-
      list(paste0("w=", default_weights),
           paste0("q", default_quantiles))
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
      suppressWarnings(arr[i,] <- RBesT::qmix(posterior, default_quantiles))
    }
    posterior_data <- data.frame(cbind(weight = default_weights, arr))
    return(posterior_data)
  }
