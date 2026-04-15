#' Filter posterior by given weights
#'
#' @description
#' Returns quantiles of the posterior distribution of the treatment effect for one or more specified weights.
#'
#' @param posterior The posterior data to be filtered (see [create_posterior_data()]).
#' @param weight The weight(s) to be filtered by.
#'
#' @return The filtered posterior values
#' 
#' @export
#'
#' @seealso [create_posterior_data()].
#'
#' @examples
#' get_posterior_by_weight(
#'   posterior = load_tipmap_data("tipPost.rds"),
#'   weight = c(0.05, 0.1)
#' )
#'
get_posterior_by_weight <- function(posterior, weight) {
  assert_that(is.data.frame(posterior), msg = "`posterior` must be a data frame")
  assert_that("weight" %in% names(posterior), msg = "`posterior` must contain a `weight` column")
  assert_that(is.numeric(weight), msg = "`weight` must be numeric")
  assert_that(all(is.finite(weight)), msg = "`weight` must be finite")
  
  weights <- weight
  
  posterior_filtered <- posterior[posterior$weight %in% weights, , drop = FALSE]
  posterior_filtered <- posterior_filtered[, setdiff(names(posterior_filtered), "weight"), drop = FALSE]
  
  posterior_filtered
}
