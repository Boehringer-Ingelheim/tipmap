#' Get cumulative probabilities from distribution of chips of one expert
#'
#' @param chips Vector of integers, representing the distribution of chips assigned by one expert, as elicited through the roulette method.
#' Each element of the vector represents one bin in the grid.
#'
#' @return A numeric vector with the cumulative distribution of chips.
#' 
#' @export
#' 
#' @seealso \code{\link{get_model_input_1exp}} and \code{\link{fit_beta_1exp}}.
#' 
#' @examples
#' chips <- c(0, 2, 3, 2, 1, 1, 1, 0, 0, 0)
#' x <- get_cum_probs_1exp(chips)
#' print(x)
#'
get_cum_probs_1exp <- function(chips) {
  # check inputs
  assert_that(is.numeric(chips))
  assert_that(all((chips - floor(chips)) == 0))
  # compute cumprobs
  sum_chips <- sum(chips)
  cum_probs <- cumsum(chips / sum_chips)
  return(cum_probs)
}
