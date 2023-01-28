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
#' @seealso \code{\link{create_new_trial_data}}, \code{\link{create_posterior_data}}, \code{\link{tipmap_plot}}, \code{\link{get_tipping_points}}
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
create_tipmap_data <-
  function(new_trial_data,
           posterior,
           map_prior,
           meta_analysis = NULL) {
    if (!(is.data.frame(posterior)))
      stop("posterior must be a data frame. Use create_posterior_data()")

    map_prior <- summary(map_prior, probs = default_quantiles)
    names(map_prior) <- c("mean", "se", paste0("q", default_quantiles))

    if (!(missing(meta_analysis))) {
      plot_data <- data.frame(
        # Graphical parameters
        x.at = c(-0.15, default_weights, 1.15, 1.35),
        # defines if data points are from target population, mix or prior
        x.col = factor(c(
          "new.obs", rep("post", length(default_weights)), rep("prior", 2)
        )),
        # treatment effect estimates for target population, posterior and prior
        t.est = c(
          new_trial_data["mean"],
          unlist(posterior["q0.5"]),
          map_prior["mean"],
          meta_analysis$TE.fixed
        ),
        t.0.025 = c(
          new_trial_data["q0.025"],
          unlist(posterior["q0.025"]),
          map_prior["q0.025"],
          meta_analysis$lower.fixed
        ),
        t.0.05 = c(NA, unlist(posterior["q0.05"]),
                   map_prior["q0.05"], NA),
        t.0.1 = c(NA, unlist(posterior["q0.1"]),
                  map_prior["q0.1"], NA),
        t.0.2 = c(NA, unlist(posterior["q0.2"]),
                  map_prior["q0.2"], NA),
        t.0.8 = c(NA, unlist(posterior["q0.8"]),
                  map_prior["q0.8"], NA),
        t.0.9 = c(NA, unlist(posterior["q0.9"]),
                  map_prior["q0.9"], NA),
        t.0.95 = c(NA, unlist(posterior["q0.95"]),
                   map_prior["q0.95"], NA),
        t.0.975 = c(
          new_trial_data["q0.975"],
          unlist(posterior["q0.975"]),
          map_prior["q0.975"],
          meta_analysis$upper.fixed
        )
      )
    } else {
      plot_data <- data.frame(
        # Graphical parameters
        x.at = c(-0.15, default_weights, 1.15),
        # defines if data points are from target population, mix or prior
        x.col = factor(c(
          "new.obs", rep("post", length(default_weights)), "prior"
        )),
        # treatment effect estimates for target population, posterior and prior
        t.est = c(new_trial_data["mean"], unlist(posterior["q0.5"]),
                  map_prior["mean"]),
        t.0.025 = c(new_trial_data["q0.025"], unlist(posterior["q0.025"]),
                    map_prior["q0.025"]),
        t.0.05 = c(new_trial_data["q0.05"], unlist(posterior["q0.05"]),
                   map_prior["q0.05"]),
        t.0.1 = c(new_trial_data["q0.1"], unlist(posterior["q0.1"]),
                  map_prior["q0.1"]),
        t.0.2 = c(new_trial_data["q0.2"], unlist(posterior["q0.2"]),
                  map_prior["q0.2"]),
        t.0.8 = c(new_trial_data["q0.8"], unlist(posterior["q0.8"]),
                  map_prior["q0.8"]),
        t.0.9 = c(new_trial_data["q0.9"], unlist(posterior["q0.9"]),
                  map_prior["q0.9"]),
        t.0.95 = c(new_trial_data["q0.95"], unlist(posterior["q0.95"]),
                   map_prior["q0.95"]),
        t.0.975 = c(new_trial_data["q0.975"], unlist(posterior["q0.975"]),
                    map_prior["q0.975"])
      )
    }
    rownames(plot_data) <- as.character(seq(1, length(plot_data$t.est)))
    return(plot_data)
  }
