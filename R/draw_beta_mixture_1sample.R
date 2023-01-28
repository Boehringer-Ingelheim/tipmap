#' Draw a single sample from a mixture of beta distributions
#'
#' @description
#' Internal function needed for expert elicitation methods.
#'
#' @param params Dataframe with parameters of beta distributions. Parameters need to be provided in columns named "alpha" and "beta", repectively, i.e. with one row per distribution).
#' @param weights Optional vector of weights assigned to experts. Defaults to equal weights.
#'
#' @return One draw (numeric value) from a mixture of beta distributions.
#'
#' @examples
#' beta_fits <- fit_beta_mult_exp(
#' chips_mult =
#'   rbind(
#'       c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
#'       c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
#'       c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
#'       )
#'   )
#' draw_beta_mixture_1sample(beta_fits)
#'
draw_beta_mixture_1sample <- function(params, weights = NULL) {
  params <- as.data.frame(params)
  n <- nrow(params)
  if (missing(weights))
    weights <- rep(1 / n, n)
  draw_component <- sample(x = 1:n,
                           size = 1,
                           prob = weights)
  draw_beta_dist <- stats::rbeta(n = 1,
                                 shape1 = params[draw_component, c("alpha")],
                                 shape2 = params[draw_component, c("beta")])
  return(draw_beta_dist)
}
