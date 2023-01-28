#' Draw n samples from a mixture of beta distributions
#'
#' @description
#' Draws samples from a mixture of beta distributions, representing pooled weights on
#' the informative component of a robust MAP prior, as elicited from experts via the roulette method.
#'
#' @param n The number of samples to be drawn.
#' @param chips_mult A data frame or matrix containing expert weights.
#' Rows should represent experts, columns should represent bins / weights.
#' @param weights An optional vector containing the weight assigned to each expert. Defaults to equal weights.
#'
#' @return A numeric vector containing samples from a pooled distribution of expert opinions.
#'
#' @examples
#' rweights <- draw_beta_mixture_nsamples(
#' n = 50,
#' chips_mult = rbind(
#'   c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
#'   c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
#'   c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
#'   ))
#' rweights
#'
draw_beta_mixture_nsamples <- function(n, chips_mult, weights = NULL) {
  samples <- numeric(length = n)
  params <- fit_beta_mult_exp(chips_mult)
  for (i in 1:n) {
    samples[i] <-
      draw_beta_mixture_1sample(params = params, weights = weights)
  }
  return(samples)
}
