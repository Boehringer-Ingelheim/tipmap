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
#' @seealso [create_new_trial_data()] and [create_posterior_data()].
#' @examples
#' prior_data <- create_prior_data(
#'   n_total = c(160, 240, 320),
#'   est = c(1.23, 1.40, 1.51),
#'   se = c(0.4, 0.36, 0.31)
#' )
create_prior_data <- function(study_label = NULL, n_total, est, se) {
  assert_that(is.numeric(n_total), msg = "`n_total` must be numeric")
  assert_that(all(is.finite(n_total)), msg = "`n_total` must be finite")
  assert_that(all(n_total == round(n_total)), msg = "`n_total` must contain whole numbers only")
  assert_that(all(n_total > 0), msg = "`n_total` must be positive")
  
  assert_that(is.numeric(est), msg = "`est` must be numeric")
  assert_that(all(is.finite(est)), msg = "`est` must be finite")
  
  assert_that(is.numeric(se), msg = "`se` must be numeric")
  assert_that(all(is.finite(se)), msg = "`se` must be finite")
  assert_that(all(se > 0), msg = "`se` must be positive")
  
  assert_that(
    length(n_total) == length(est) && length(est) == length(se),
    msg = "`n_total`, `est`, and `se` must have the same length"
  )
  
  if (is.null(study_label)) {
    study_label <- paste("Study", seq_along(est))
  } else {
    assert_that(is.atomic(study_label), msg = "`study_label` must be an atomic vector")
    assert_that(length(study_label) == length(est), msg = "`study_label` must have the same length as `est`")
  }
  
  data.frame(
    study_label = as.character(study_label),
    n_total = n_total,
    est = est,
    se = se,
    stringsAsFactors = FALSE
  )
}
