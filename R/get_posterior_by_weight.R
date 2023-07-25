#' Filter posterior by given weights
#'
#' @description
#' Returns quantiles of the posterior distribution of the treatment effect for one or more specified weights.
#'
#' @param posterior The posterior data to be filtered (see \code{create_posterior_data()}).
#' @param weight The weight(s) to be filtered by.
#'
#' @return The filtered posterior values
#' 
#' @export
#'
#' @seealso \code{\link{create_posterior_data}}
#'
#' @examples
#' get_posterior_by_weight(
#'   posterior = load_tipmap_data("tipPost.rds"),
#'   weight = c(0.05, 0.1)
#' )
#'
get_posterior_by_weight <- function(posterior, weight) {
  if (!(is.numeric(weight))) stop("Weight must be numeric.")
  weights <- weight
  posterior_filtered <- dplyr::filter(posterior, weight %in% weights)
  posterior_filtered <- dplyr::select(posterior_filtered, -weight)
  return(posterior_filtered)
}
