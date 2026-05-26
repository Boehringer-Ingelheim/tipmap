#' Identify tipping point for a specific quantile.
#'
#' @description
#' Identifies the weights closest to tipping points for specified quantiles.
#'
#' @param tipmap_data A data frame created by \code{create_tipmap_data()}.
#' @param quantile The quantile(s) of the tipping point. Possible values are 0.025, 0.05, 0.1, 0.2, 0.8, 0.9, 0.95 and 0.975.
#' @param null_effect The null treatment effect. Defaults to 0.
#'
#' @return The weight closest to the tipping point for the specified quantile
#' @export
#' 
#' @seealso [create_tipmap_data()].
#' 
#' @examples
#' tip_dat <- load_tipmap_data("tipdat.rds")#'
#' get_tipping_points(tip_dat, quantile = 0.025)
#' get_tipping_points(tip_dat, quantile = c(0.025, 0.05, 0.1, 0.2), null_effect = 0.1)
#'
get_tipping_points <- function(tipmap_data, quantile, null_effect = 0) {
  allowed_quantiles <- default_quantiles[default_quantiles != 0.5]
  
  assert_that(is.data.frame(tipmap_data), msg = "`tipmap_data` must be a data frame. See create_tipmap_data()")
  assert_that(is.numeric(quantile), msg = "`quantile` must be numeric")
  assert_that(all(quantile %in% allowed_quantiles), msg = "`quantile` must be one of the default quantiles except 0.5")
  assert_that(is.numeric(null_effect), msg = "`null_effect` must be numeric")
  assert_that(length(null_effect) == 1, msg = "`null_effect` must be length 1")
  assert_that(is.finite(null_effect), msg = "`null_effect` must be finite")
  
  required_cols <- c("x.at", paste0("t.", quantile))
  assert_that(all(required_cols %in% names(tipmap_data)), msg = "`tipmap_data` does not contain the required quantile column(s)")
  
  tp <- numeric(length(quantile))
  
  for (i in seq_along(quantile)) {
    column <- paste0("t.", quantile[i])
    idx <- which(abs(tipmap_data[[column]] - null_effect) == min(abs(tipmap_data[[column]] - null_effect), na.rm = TRUE))
    tp[i] <- as.numeric(tipmap_data[idx[1], "x.at"])
  }
  
  for (i in seq_along(tp)) {
    if (tp[i] == 0) {
      message(paste0("Weight 0 identified for tipping point of quantile ", quantile[i]))
    } else if (tp[i] >= 1) {
      message(paste0("Weight >=1 identified for tipping point of quantile ", quantile[i]))
    }
  }
  
  if (any(tp >= 1)) {
    tp[tp >= 1] <- NA_real_
  }
  
  names(tp) <- paste0("q", quantile)
  tp
}