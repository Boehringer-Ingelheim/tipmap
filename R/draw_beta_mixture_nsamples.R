#' @title 
#' Draw samples from a mixture of beta distributions
#'
#' @description
#' Draws samples from a mixture of beta distributions, representing pooled weights on the informative component of a robust MAP prior, as elicited from experts via the roulette method.
#'
#' @param n Numeric value, the number of samples to be drawn.
#' @param chips_mult Numeric matrix, containing expert weighting (distributions of chips). Rows should represent experts, columns should represent bins / weight intervals.
#' @param expert_weight An optional numeric vector, containing the weight assigned to each expert (defaults to equal weights).
#'
#' @return A numeric vector containing samples from a pooled distribution of expert opinions.
#' 
#' @export
#' 
#' @seealso \code{\link{fit_beta_mult_exp}} and \code{\link{get_summary_mult_exp}}.
#' 
#' @examples
#' rweights <- draw_beta_mixture_nsamples(
#'   n = 50,
#'   chips_mult = rbind(
#'     c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
#'     c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
#'     c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
#'   ),
#'   expert_weight = rep(1/3, 3)
#' )
#' print(rweights)
#' \dontrun{
#' hist(rweights)
#' }
draw_beta_mixture_nsamples <- function(
    n, chips_mult, expert_weight = NULL
  ) {
  # check inputs
  assert_that(is.count(n))
  assert_that(is.matrix(chips_mult))
  assert_that(is.numeric(chips_mult))
  if (missing(expert_weight)) {
    expert_weight <- rep(1 / nrow(chips_mult), nrow(chips_mult)) 
  }
  # function to draw 1 sample
  draw_beta_mixture_1sample <- function(params, expert_weight) {
    params <- as.data.frame(params)
    n_comp <- nrow(params)
    comp_draw <- sample(
      x = 1:n_comp,
      size = 1,
      prob = expert_weight
    )
    draw_beta_dist <- stats::rbeta(
      n = 1,
      shape1 = params[comp_draw, c("alpha")],
      shape2 = params[comp_draw, c("beta")]
    )
    return(draw_beta_dist)
  }
  # draw n samples
  samples <- numeric(length = n)
  params <- fit_beta_mult_exp(chips_mult)
  for (i in 1:n) {
    samples[i] <- draw_beta_mixture_1sample(
      params = params, 
      expert_weight = expert_weight
    )
  }
  return(samples)
}
