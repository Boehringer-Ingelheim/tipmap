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
get_summary_mult_exp <- function(
    chips_mult,
    n = 500,
    expert_weight = NULL) {
  # check inputs
  assert_that(is.matrix(chips_mult))
  assert_that(is.numeric(chips_mult))
  assert_that(is.count(n))
  # compute summary statistics
  if (missing(expert_weight))
    expert_weight <- rep(1 / nrow(chips_mult), nrow(chips_mult))
  samples <- draw_beta_mixture_nsamples(
    n = n, 
    chips_mult = chips_mult, 
    expert_weight = expert_weight
  )
  return(base::summary(samples))
  }
