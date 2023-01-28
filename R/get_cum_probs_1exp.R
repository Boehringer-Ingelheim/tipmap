#' Get cumulative probabilities from distribution of chips of one expert
#'
#' @description
#' Internal function needed for expert elicitation methods.
#'
#' @param chips A numeric vector representing the distribution of chips of one expert.
#' The vector must be of length 10 and contents must add up to 10.
#' The first column represents weight on interval 0 to 0.1.
#'
#' @return A vector of cumulative probabilities.
#'
#' @examples
#' chips <- c(0,2,3,2,1,1,1,0,0,0)
#' x <- get_cum_probs_1exp(chips)
#' x
#'
get_cum_probs_1exp <- function(chips) {
  n_chips <- 10
  if (!is.numeric(chips)) chips <- as.numeric(chips)
  if (length(chips) < n_chips) stop("Length of vector must be 10.")
  if (sum(chips) != n_chips) stop("Integer values must add up to 10.")
  cum_probs <- cumsum(chips / length(chips))
  return(cum_probs)
}
