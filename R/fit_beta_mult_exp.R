#' @title
#' Fit beta distributions for multiple experts
#'
#' @description
#' Fit beta distributions to data elicited from multiple experts via the roulette method.
#'
#' @param chips_mult A dataframe or matrix containing weights.
#' It should contain one row per expert and 10 columns, one for each bin, representing weights from 0 to 1.
#'
#' @return A dataframe containing the parameters of the individual beta distributions.
#' 
#' @export
#' 
#' @seealso \code{\link{fit_beta_1exp}}.
#' 
#' @examples
#' beta_fits <- fit_beta_mult_exp(
#'   chips_mult = rbind(
#'     c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
#'     c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
#'     c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
#'   )
#' )
#' print(beta_fits)
#'
fit_beta_mult_exp <- function(chips_mult) {
  chips_mult <- purrr::array_branch(array = chips_mult, margin = 1)
  dat <- purrr::map(
    .x = chips_mult,
    .f = ~ get_model_input_1exp(cum_probs = get_cum_probs_1exp(.x))
  )
  alpha <- beta <- value <- convergence <- NULL
  beta_fits <- purrr::map(.x = dat,
                          .f = ~ fit_beta_1exp(df = .x))
  beta_fits <- purrr::map(beta_fits, unlist)
  beta_fits <- dplyr::bind_rows(beta_fits)
  names(beta_fits)[1:2] <- c("alpha", "beta")
  beta_fits$convergence <-
    ifelse(beta_fits$convergence == 0, TRUE, FALSE)
  beta_fits <- dplyr::select(beta_fits, alpha, beta,
                             "error" = value, convergence)
  return(beta_fits)
}
