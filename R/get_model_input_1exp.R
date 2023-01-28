#' Transform cumulative probabilities to fit beta distributions
#'
#' @description
#' Internal function needed for expert elicitation methods.
#'
#' @param cum_probs Cumulative probabilities of weights of one expert.
#' @param w Weight of bins.
#'
#' @return Dataframe that can be used as input to fit beta distributions by
#' \code{fit_beta_1exp()}.
#'
#' @examples
#' chips <- c(0,2,3,2,1,1,1,0,0,0)
#' x <- get_cum_probs_1exp(chips)
#' y <- get_model_input_1exp(x)
#' y
#'
get_model_input_1exp <- function(cum_probs, w = NULL) {
  if (is.null(w)) w <- 1:10 / 10
  dat <- data.frame(w = w, cum_probs = cum_probs)
  dat <- subset(dat, cum_probs > 0)
  dat <- dat[match(unique(dat$cum_probs), dat$cum_probs), ]
  row.names(dat) <- NULL
  return(dat)
}
