#' Get cumulative probabilities from distribution of chips of one expert
#'
#' @param chips Vector of integers, representing the distribution of chips assigned by one expert, as elicited through the roulette method.
#' Each element of the vector represents one bin in the grid.
#'
#' @return A numeric vector with the cumulative distribution of chips.
#' 
#' @export
#' 
#' @seealso [get_model_input_1exp()] and [fit_beta_1exp()].
#' 
#' @examples
#' chips <- c(0, 2, 3, 2, 1, 1, 1, 0, 0, 0)
#' x <- get_cum_probs_1exp(chips)
#' print(x)
#'
get_cum_probs_1exp <- function(chips) {
  # check inputs
  assert_that(
    is.numeric(chips) && all(is.finite(chips)),
    msg = "`chips` must contain finite numeric values only."
  )
  assert_that(
    all((chips - floor(chips)) == 0),
    msg = "`chips` must contain whole numbers only."
  )
  assert_that(
    all(chips >= 0),
    msg = "`chips` must contain non-negative values only."
  )
  assert_that(
    sum(chips) > 0,
    msg = "`chips` must contain at least one positive value."
  )
  # compute cumprobs
  sum_chips <- sum(chips)
  cum_probs <- cumsum(chips / sum_chips)
  # Clamp to [0, 1] to absorb floating-point accumulation error.
  # By construction the true final value is 1; cumsum() can produce values
  # marginally above 1.0 on certain FPU configurations (e.g. noLD, aarch64).
  cum_probs <- pmin(pmax(cum_probs, 0), 1)
  return(cum_probs)
}
