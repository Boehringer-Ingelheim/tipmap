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
#' @seealso \code{\link{create_posterior_data}}, \code{\link{create_tipmap_data}}
#' @examples
#' new_trial_data <- create_new_trial_data(
#'   n_total = 30, est = 1.27, se = 0.95
#' )
#'
create_new_trial_data <- function(n_total, est, se) {
  if (!(n_total == round(n_total)))
    stop("`n_total` must be a whole number")
  if (!(is.numeric(est)))
    stop("`est` must be numeric")
  if (!(is.numeric(se)))
    stop("`se` must be numeric")
  if ((se <= 0))
    stop("`se` must be positive")
  if ((length(n_total) != 1) ||
      (length(est) != 1) || (length(se) != 1)) {
    stop("Argments must be of length 1")
  }
  new_trial_data <- c(n_total, est, se,
                      stats::qnorm(p = default_quantiles,
                                   mean = est,
                                   sd = se))
  names(new_trial_data) <-
    c("n_total", "mean", "se", paste0("q", default_quantiles))
  return(new_trial_data)
}
