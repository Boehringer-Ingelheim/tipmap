#' Data on new trial in target population
#'
#' Creates a vector containing data on the new trial in the target population. This may be hypothetical data in the planning stage.
#'
#' @param n_total The total sample size.
#' @param est Treatment effect estimate.
#' @param se Standard error of the treatment effect estimate.
#'
#' @return A numeric vector with data on the new trial, incl. quantiles of an assumed normal data likelihood.
#' @export
#' @seealso [create_posterior_data()]  and [create_tipmap_data()].
#' @examples
#' new_trial_data <- create_new_trial_data(
#'   n_total = 30, est = 1.27, se = 0.95
#' )
#'
create_new_trial_data <- function(n_total, est, se) {
  assert_that(is.numeric(n_total), msg = "`n_total` must be numeric")
  assert_that(length(n_total) == 1, msg = "`n_total` must be length 1")
  assert_that(is.finite(n_total), msg = "`n_total` must be finite")
  assert_that(n_total == round(n_total), msg = "`n_total` must be a whole number")
  assert_that(n_total > 0, msg = "`n_total` must be positive")
  
  assert_that(is.numeric(est), msg = "`est` must be numeric")
  assert_that(length(est) == 1, msg = "`est` must be length 1")
  assert_that(is.finite(est), msg = "`est` must be finite")
  
  assert_that(is.numeric(se), msg = "`se` must be numeric")
  assert_that(length(se) == 1, msg = "`se` must be length 1")
  assert_that(is.finite(se), msg = "`se` must be finite")
  assert_that(se > 0, msg = "`se` must be positive")
  
  new_trial_data <- c(
    n_total,
    est,
    se,
    stats::qnorm(p = default_quantiles, mean = est, sd = se)
  )
  
  names(new_trial_data) <- c(
    "n_total", "mean", "se", paste0("q", default_quantiles)
  )
  
  new_trial_data
}
