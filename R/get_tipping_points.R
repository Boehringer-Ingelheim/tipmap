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
#' @seealso \code{\link{create_tipmap_data}}
#' @examples
#' tip_dat <- load_tipmap_data("tipdat.rds")#'
#' get_tipping_points(tip_dat, quantile = 0.025)
#' get_tipping_points(tip_dat, quantile = c(0.025, 0.05, 0.1, 0.2), null_effect = 0.1)
#'
get_tipping_points <-
  function(tipmap_data, quantile, null_effect = 0) {
    if (!(is.numeric(quantile)))
      stop("`quantile` must be numeric and in c(0.025, 0.05, 0.1, 0.2, 0.8, 0.9, 0.95, 0.975)")
    if (!(all(quantile %in% default_quantiles[-7])))
      stop("`quantile` must be in c(0.025, 0.05, 0.1, 0.2, 0.8, 0.9, 0.95, 0.975)")
    if (!(is.numeric(null_effect)))
      stop("`null_effect` must be numeric")
    if (!(is.data.frame(tipmap_data)))
      stop("`tipmap_data` must be a data frame. See create_tipmap_data()")

    column <- character(length = 1)
    tp <- numeric(length = length(quantile))

    for (i in 1:length(tp)) {
      column <- paste0("t.", as.character(quantile[i]))
      tp[i] <-
        as.numeric((tipmap_data[which(abs(tipmap_data[column] - null_effect) ==
                                        min(abs(tipmap_data[column] - null_effect),
                                            na.rm = TRUE)), ]["x.at"]))
    }

    for (i in 1:length(tp)) {
      if (tp[i] == 0) {
        message(paste0(
          "Weight 0 identified for tipping point of quantile ",
          quantile[i]
        ))
      } else if (tp[i] >= 1) {
        message(paste0(
          "Weight 1 identified for tipping point of quantile ",
          quantile[i]
        ))
      }
    }

    if (length(which(tp >= 1) > 0)) {
      tp[which(tp >= 1)] <- NA
    }

    names(tp) <- paste0("q", quantile)
    return(tp)
  }
