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
#' @seealso [fit_beta_mult_exp()] and [get_summary_mult_exp()].
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
draw_beta_mixture_nsamples <- function(n, chips_mult, expert_weight = NULL) {
  assert_that(is.count(n), msg = "`n` must be a positive whole number")
  assert_that(is.matrix(chips_mult) || is.data.frame(chips_mult), msg = "`chips_mult` must be a matrix or data frame")
  
  chips_mult <- as.matrix(chips_mult)
  
  assert_that(is.numeric(chips_mult), msg = "`chips_mult` must be numeric")
  assert_that(nrow(chips_mult) >= 1, msg = "`chips_mult` must have at least one row")
  
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
  
  params <- fit_beta_mult_exp(chips_mult)
  comp_draw <- sample(seq_len(nrow(params)), size = n, replace = TRUE, prob = expert_weight)
  stats::rbeta(n = n, shape1 = params$alpha[comp_draw], shape2 = params$beta[comp_draw])
}