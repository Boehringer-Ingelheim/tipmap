#' @title 
#' Summarize expert weights
#'
#' @description
#' Computes minimum, maximum, mean and quartiles for expert weights.
#'
#' @param chips_mult Numeric matrix, containing expert weights.
#' @param n Number of samples to be drawn to obtain summary statistics (defaults to 500).
#' @param expert_weight Weights assigned to each expert (defaults to equal weights).
#'
#' @return A vector containing summary statistics.
#' @export
#' @examples
#' get_summary_mult_exp(
#'   chips_mult = rbind(
#'     c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
#'     c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
#'     c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
#'   ), 
#'   n = 50
#' )
#'
get_summary_mult_exp <- function(chips_mult, n = 500, expert_weight = NULL) {
  assert_that(is.matrix(chips_mult) || is.data.frame(chips_mult), msg = "`chips_mult` must be a matrix or data frame")
  chips_mult <- as.matrix(chips_mult)
  assert_that(is.numeric(chips_mult), msg = "`chips_mult` must be numeric")
  assert_that(is.count(n), msg = "`n` must be a positive whole number")
  
  if (is.null(expert_weight)) {
    expert_weight <- rep(1 / nrow(chips_mult), nrow(chips_mult))
  } else {
    assert_that(is.numeric(expert_weight), msg = "`expert_weight` must be numeric")
    assert_that(length(expert_weight) == nrow(chips_mult), msg = "`expert_weight` must have one entry per expert")
    assert_that(all(is.finite(expert_weight)), msg = "`expert_weight` must be finite")
    assert_that(all(expert_weight >= 0), msg = "`expert_weight` must be non-negative")
    assert_that(sum(expert_weight) > 0, msg = "`expert_weight` must sum to a positive value")
    expert_weight <- expert_weight / sum(expert_weight)
  }
  
  samples <- draw_beta_mixture_nsamples(
    n = n,
    chips_mult = chips_mult,
    expert_weight = expert_weight
  )
  
  base::summary(samples)
}
