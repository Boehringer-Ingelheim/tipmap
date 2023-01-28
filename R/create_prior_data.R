#' Creates input data frame for construction of MAP prior
#'
#' Assembling information from trials in the source population in a structured way (required as a pre-processing step for MAP prior creation).
#'
#' @param study_label An optional vector containing trial labels.
#' @param n_total A vector containing total sample sizes.
#' @param est A vector containing treatment effect estimates.
#' @param se A vector containing standard errors of the effect estimates.
#'
#' @return A data frame containing data on the trials in the source population.
#' @export
#' @seealso \code{\link{create_new_trial_data}}, \code{\link{create_posterior_data}}
#' @examples
#' prior_data <- create_prior_data(
#'   n_total = c(160, 240, 320),
#'   est = c(1.23, 1.40, 1.51),
#'   se = c(0.4, 0.36, 0.31)
#' )
create_prior_data <-
  function(study_label = NULL,
           n_total,
           est,
           se) {
    if ((any(n_total != round(n_total))))
      stop("`n_total` must be a whole number")
    if ((any(n_total <= 0)))
      stop("`n_total` must be positive")
    if (!(is.numeric(est)))
      stop("`est` must be numeric")
    if (!(is.numeric(se)))
      stop("`se` must be numeric")
    if ((any(se <= 0)))
      stop("`se` must be positive")

    if (missing(study_label)) {
      study_label <-
        paste("Study", seq(
          from = 1,
          to = length(est),
          by = 1
        ))
    }

    prior_data <-
      data.frame(study_label, n_total, est, se)
    return(prior_data)
  }
