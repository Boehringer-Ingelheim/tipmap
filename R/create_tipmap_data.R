#' Create data frame ready to use for tipping point analysis
#'
#' Combines new trial data created by \code{createTargetData()}, a posterior distribution created by \code{create_posterior_data()} and a
#' robust MAP prior using \code{RBesT::automixfit()} and an optional meta-analysis, e.g. created using the \code{meta} package, into a data frame
#' needed for the functions \code{tipmap_plot()} and \code{get_tipping_point()}.
#'
#' @param new_trial_data A data frame containing data on the new trial in the target population. See \code{create_new_trial_data()}.
#' @param posterior A mixture combining MAP prior and target population. See \code{create_posterior_data()}.
#' @param map_prior A robust MAP prior created by \code{RBesT::automixfit()}.
#' @param meta_analysis A data frame containing a meta-analysis of trial(s) to be borrowed from. See \code{createPriorData()}.
#'
#' @return A data frame ready to be used for \code{tipmap_plot()} and \code{get_tipping_point()}
#' @export
#' 
#' @seealso 
#' [create_new_trial_data()], [create_posterior_data()], 
#' [tipmap_plot()] and [get_tipping_points()].
#' 
#' @examples
#'
#' # specify new trial data
#' new_trial_data <- create_new_trial_data(n_total = 30, est = 1.5, se = 2.1)
#'
#' # read MAP prior data
#' map_prior <- load_tipmap_data("tipmapPrior.rds")
#'
#' # read posterior data
#' posterior <- load_tipmap_data("tipPost.rds")
#'
#' tip_dat <- create_tipmap_data(
#'   new_trial_data = new_trial_data,
#'   posterior = posterior,
#'   map_prior = map_prior
#' )
#'
create_tipmap_data <- function(
    new_trial_data,
    posterior,
    map_prior,
    meta_analysis = NULL
) {
  posterior_cols <- paste0("q", default_quantiles)
  new_trial_required <- c("mean", posterior_cols)
  meta_required <- c("TE.fixed", "lower.fixed", "upper.fixed")
  
  assert_that(is.numeric(new_trial_data), msg = "`new_trial_data` must be numeric")
  assert_that(!is.null(names(new_trial_data)), msg = "`new_trial_data` must be named")
  assert_that(
    all(new_trial_required %in% names(new_trial_data)),
    msg = "`new_trial_data` does not contain the required quantile entries"
  )
  
  assert_that(is.data.frame(posterior), msg = "`posterior` must be a data frame. Use create_posterior_data()")
  assert_that(
    all(c("weight", posterior_cols) %in% names(posterior)),
    msg = "`posterior` must contain `weight` and all default quantile columns"
  )
  
  assert_that(
    "normMix" %in% class(map_prior),
    msg = "`map_prior` must be a normal mixture prior, e.g. from RBesT"
  )
  
  if (!is.null(meta_analysis)) {
    assert_that(is.data.frame(meta_analysis), msg = "`meta_analysis` must be a data frame")
    assert_that(
      all(meta_required %in% names(meta_analysis)),
      msg = "`meta_analysis` must contain `TE.fixed`, `lower.fixed`, and `upper.fixed`"
    )
    assert_that(nrow(meta_analysis) >= 1, msg = "`meta_analysis` must have at least one row")
  }
  
  map_prior_summary <- summary(map_prior, probs = default_quantiles)
  names(map_prior_summary) <- c("mean", "se", paste0("q", default_quantiles))
  
  if (!is.null(meta_analysis)) {
    plot_data <- data.frame(
      x.at = c(-0.15, default_weights, 1.15, 1.35),
      x.col = factor(c("new.obs", rep("post", length(default_weights)), rep("prior", 2))),
      t.est = c(
        new_trial_data["mean"],
        unlist(posterior["q0.5"]),
        map_prior_summary["mean"],
        meta_analysis$TE.fixed[1]
      ),
      t.0.025 = c(
        new_trial_data["q0.025"],
        unlist(posterior["q0.025"]),
        map_prior_summary["q0.025"],
        meta_analysis$lower.fixed[1]
      ),
      t.0.05 = c(NA, unlist(posterior["q0.05"]), map_prior_summary["q0.05"], NA),
      t.0.1 = c(NA, unlist(posterior["q0.1"]), map_prior_summary["q0.1"], NA),
      t.0.2 = c(NA, unlist(posterior["q0.2"]), map_prior_summary["q0.2"], NA),
      t.0.8 = c(NA, unlist(posterior["q0.8"]), map_prior_summary["q0.8"], NA),
      t.0.9 = c(NA, unlist(posterior["q0.9"]), map_prior_summary["q0.9"], NA),
      t.0.95 = c(NA, unlist(posterior["q0.95"]), map_prior_summary["q0.95"], NA),
      t.0.975 = c(
        new_trial_data["q0.975"],
        unlist(posterior["q0.975"]),
        map_prior_summary["q0.975"],
        meta_analysis$upper.fixed[1]
      )
    )
  } else {
    plot_data <- data.frame(
      x.at = c(-0.15, default_weights, 1.15),
      x.col = factor(c("new.obs", rep("post", length(default_weights)), "prior")),
      t.est = c(new_trial_data["mean"], unlist(posterior["q0.5"]), map_prior_summary["mean"]),
      t.0.025 = c(new_trial_data["q0.025"], unlist(posterior["q0.025"]), map_prior_summary["q0.025"]),
      t.0.05 = c(new_trial_data["q0.05"], unlist(posterior["q0.05"]), map_prior_summary["q0.05"]),
      t.0.1 = c(new_trial_data["q0.1"], unlist(posterior["q0.1"]), map_prior_summary["q0.1"]),
      t.0.2 = c(new_trial_data["q0.2"], unlist(posterior["q0.2"]), map_prior_summary["q0.2"]),
      t.0.8 = c(new_trial_data["q0.8"], unlist(posterior["q0.8"]), map_prior_summary["q0.8"]),
      t.0.9 = c(new_trial_data["q0.9"], unlist(posterior["q0.9"]), map_prior_summary["q0.9"]),
      t.0.95 = c(new_trial_data["q0.95"], unlist(posterior["q0.95"]), map_prior_summary["q0.95"]),
      t.0.975 = c(new_trial_data["q0.975"], unlist(posterior["q0.975"]), map_prior_summary["q0.975"])
    )
  }
  
  rownames(plot_data) <- as.character(seq_len(nrow(plot_data)))
  plot_data
}