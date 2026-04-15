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
  assert_that(is.numeric(chips), msg = "`chips` must be numeric")
  assert_that(length(chips) > 0, msg = "`chips` must not be empty")
  assert_that(all(is.finite(chips)), msg = "`chips` must be finite")
  assert_that(all((chips - floor(chips)) == 0), msg = "`chips` must contain whole numbers only")
  assert_that(all(chips >= 0), msg = "`chips` must be non-negative")
  assert_that(sum(chips) > 0, msg = "`chips` must contain at least one positive value")
  
  cumsum(chips / sum(chips))
}
