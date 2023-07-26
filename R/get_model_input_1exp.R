#' Transform cumulative probabilities to fit beta distributions
#'
#' @param cum_probs Numeric vector, containing cumulative probabilities of weights for one expert, as elicited through the roulette method. Each element of the vector represents one bin in the grid.
#' @param w Numeric vector, upper interval limit of bin (defaults to \code{1:length(cum_probs) / length(cum_probs)}).
#'
#' @return Dataframe to be used as input to fit beta distributions by \code{\link{fit_beta_1exp}}.
#'
#' @export
#' 
#' @seealso \code{\link{get_cum_probs_1exp}} and \code{\link{fit_beta_1exp}}.
#'
#' @examples
#' chips <- c(0, 2, 3, 2, 1, 1, 1, 0, 0, 0)
#' x <- get_cum_probs_1exp(chips)
#' print(x)
#' y <- get_model_input_1exp(x)
#' print(y)
#' 
get_model_input_1exp <- function(cum_probs, w = NULL) {
  # check inputs
  assert_that(is.numeric(cum_probs))
  assert_that(all(cum_probs >= 0))
  assert_that(all(cum_probs <= 1))
  assert_that(all(cum_probs == cummax(cum_probs)))
  # create dataframe
  w <- 1:length(cum_probs) / length(cum_probs)
  dat <- data.frame(w = w, cum_probs = cum_probs)
  dat <- subset(dat, cum_probs > 0)
  dat <- dat[match(unique(dat$cum_probs), dat$cum_probs), ]
  row.names(dat) <- NULL
  return(dat)
}
