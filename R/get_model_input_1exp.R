#' Transform cumulative probabilities to fit beta distributions
#'
#' @param cum_probs Numeric vector, containing cumulative probabilities of weights for one expert, as elicited through the roulette method. Each element of the vector represents one bin in the grid.
#' @param w Numeric vector, upper interval limit of bin (defaults to \code{1:length(cum_probs) / length(cum_probs)}).
#'
#' @return Dataframe to be used as input to fit beta distributions by [fit_beta_1exp()].
#'
#' @export
#' 
#' @seealso [get_cum_probs_1exp()] and [fit_beta_1exp()].
#'
#' @examples
#' chips <- c(0, 2, 3, 2, 1, 1, 1, 0, 0, 0)
#' x <- get_cum_probs_1exp(chips)
#' print(x)
#' y <- get_model_input_1exp(x)
#' print(y)
#' 
get_model_input_1exp <- function(cum_probs, w = NULL) {
  assert_that(is.numeric(cum_probs), msg = "`cum_probs` must be numeric")
  assert_that(length(cum_probs) > 0, msg = "`cum_probs` must not be empty")
  assert_that(all(is.finite(cum_probs)), msg = "`cum_probs` must be finite")
  assert_that(all(cum_probs >= 0 & cum_probs <= 1), msg = "`cum_probs` must lie in [0, 1]")
  assert_that(all(diff(cum_probs) >= 0), msg = "`cum_probs` must be non-decreasing")
  
  if (is.null(w)) {
    w <- seq_along(cum_probs) / length(cum_probs)
  } else {
    assert_that(is.numeric(w), msg = "`w` must be numeric")
    assert_that(length(w) == length(cum_probs), msg = "`w` must have the same length as `cum_probs`")
    assert_that(all(is.finite(w)), msg = "`w` must be finite")
  }
  
  dat <- data.frame(w = w, cum_probs = cum_probs)
  dat <- subset(dat, cum_probs > 0)
  dat <- dat[match(unique(dat$cum_probs), dat$cum_probs), , drop = FALSE]
  row.names(dat) <- NULL
  
  assert_that(nrow(dat) > 0, msg = "No usable rows remain after preprocessing `cum_probs`")
  
  dat
}
