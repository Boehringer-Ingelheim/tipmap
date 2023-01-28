#' Summarize expert weights
#'
#' @description
#' Computes minimum, maximum, mean and quartiles for expert weights.
#'
#' @param chips_mult A data frame or matrix containing expert weights.
#' @param n The number of samples to be drawn to obtain summary statistics. Defaults to 500.
#' @param weights Weights assigned to each expert. Defaults to equal weights.
#'
#' @return A vector containing summary statistics.
#'
#' @examples
#' get_summary_mult_exp(rbind(
#'   c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
#'   c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
#'   c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
#'   ))
#'
get_summary_mult_exp <- function(
    chips_mult,
    n = 500,
    weights = NULL) {
  if (missing(weights))
    weights <- rep(1 / nrow(chips_mult), nrow(chips_mult))
  samples <- draw_beta_mixture_nsamples(n = n, chips_mult, weights = weights)
  return(base::summary(samples))
  }
