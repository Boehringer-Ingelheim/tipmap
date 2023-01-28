## ----setup, include = F-------------------------------------------------------
knitr::opts_chunk$set(
  echo = T, collapse = T, warning = F, message = F, 
  prompt = T, comment = "#",
  out.width = "100%"
)

## ---- eval=T, echo=T----------------------------------------------------------
library(tipmap)
prior_data <- create_prior_data(
  n_total = c(160, 240, 320),
  est = c(1.16, 1.43, 1.59),
  se = c(0.46, 0.35, 0.28)
)

## ---- eval=F, echo=F----------------------------------------------------------
#  # compute standard deviation of change in each arm (assumed equal);
#  # for two-sample data:
#  sd <- prior_data$se / sqrt(1/(prior_data$n_total/2) + 1/(prior_data$n_total/2))
#  sigma1 <- mean(sd)
#  sigma1 # = 2.70826
#  nt <- 15; nc <- 15
#  f <- (nt+nc)^2 / (nt*nc)
#  sigma2 <- sqrt(f)*sigma1
#  sigma2 # = 5.41652

## ---- eval=T, echo=T----------------------------------------------------------
print(prior_data)

## ---- eval=T, echo=T----------------------------------------------------------
set.seed(123)
uisd <- 5.42
map_mcmc <- RBesT::gMAP(
  formula = cbind(est, se) ~ 1 | study_label,
  data = prior_data,
  family = gaussian,
  weights = n_total,
  tau.dist = "HalfNormal",
  tau.prior = cbind(0, uisd / 16),
  beta.prior = cbind(0, uisd)
  )

## ---- eval=T, echo=T----------------------------------------------------------
summary(map_mcmc)

## ----forest_plot, eval=T, echo=T, fig.width=6, fig.height=3, dev=c('png','pdf'), out.width="70%", fig.cap='Figure 1: Forest plot.'----
plot(map_mcmc)$forest_model

## ---- eval=T, echo=T----------------------------------------------------------
map_prior <- RBesT::automixfit(
  sample = map_mcmc,
  Nc = seq(1, 4),
  k = 6,
  thresh = -Inf
  )

## ---- eval=T, echo=T----------------------------------------------------------
print(map_prior)

## ----map_prior_dens, eval=T, echo=T, fig.width=6, fig.height=3, dev=c('png','pdf'), out.width="70%", fig.cap='Figure 2: Overlay of the MCMC histogram of the MAP prior and the fitted parametric mixture approximation.'----
plot(map_prior)$mix

## ---- eval=T, echo=T----------------------------------------------------------
pediatric_trial <- create_new_trial_data(n_total = 30, est = 1.02, se = 1.4)

## ---- eval=T, echo=T----------------------------------------------------------
print(pediatric_trial)

## ---- eval=T, echo=T----------------------------------------------------------
posterior <- create_posterior_data(
  map_prior = map_prior,
  new_trial_data = pediatric_trial,
  sigma = uisd)

## ---- eval=T, echo=T----------------------------------------------------------
head(posterior, 4)

## ---- eval=T, echo=T----------------------------------------------------------
tail(posterior, 4)

## ---- eval=F, echo=F----------------------------------------------------------
#  length(posterior$weight)
#  dim(posterior)[1]
#  class(posterior)
#  colnames(posterior)[-1]

## ---- eval=T, echo=T----------------------------------------------------------
colnames(posterior)[-1]

## ---- eval=T, echo=T----------------------------------------------------------
tipmap_data <- create_tipmap_data(
  new_trial_data = pediatric_trial,
  posterior = posterior,
  map_prior = map_prior)

## ----tipmap_plot, eval=T, echo=T, fig.width=8, fig.height=5, dev=c('png','pdf'), out.width="95%", fig.cap='Figure 3: Tipping point plot.'----
(p1 <- tipmap_plot(tipmap_data = tipmap_data))

## ----tipmap_plot_refline, eval=T, echo=T, fig.width=8, fig.height=5, dev=c('png','pdf'), out.width="95%", fig.cap='Figure 4: Tipping point plot with reference line.'----
primary_weight <- 0.38
(p2 <- p1 + ggplot2::geom_vline(xintercept = primary_weight, col="green4"))

## ---- eval=T, echo=T----------------------------------------------------------
get_posterior_by_weight(
  posterior = posterior, 
  weight = c(primary_weight)
  )

## ---- eval=T, echo=T----------------------------------------------------------
tipp_points <- get_tipping_points(
  tipmap_data = tipmap_data,  
  quantile = c(0.2, 0.1, 0.05, 0.025)
)
tipp_points

## ---- eval=T, echo=T----------------------------------------------------------
prior_primary <- RBesT::robustify(
  priormix = map_prior,
  weight = (1 - primary_weight),
  m = 0,
  n = 1,
  sigma = uisd
  )

## ---- eval=T, echo=T----------------------------------------------------------
posterior_primary <- RBesT::postmix(
  priormix = prior_primary,
  m = pediatric_trial["mean"],
  se = pediatric_trial["se"]
  )

## ---- eval=F, echo=F----------------------------------------------------------
#  summary(posterior_primary)

## ---- eval=T, echo=T----------------------------------------------------------
round(1 - RBesT::pmix(posterior_primary, q = 0), 3)
round(1 - RBesT::pmix(posterior_primary, q = 0.5), 3)
round(1 - RBesT::pmix(posterior_primary, q = 1), 3)

## ----cumulative_dens, eval=T, echo=T, fig.width=7, fig.height=4.5, dev=c('png','pdf'), out.width="80%", fig.cap='Figure 5: Cumulative density of posterior with weight w=0.38.'----
library(ggplot2)
plot(posterior_primary, fun = RBesT::pmix) +
  scale_x_continuous(breaks = seq(-1, 2, 0.5)) +
  scale_y_continuous(breaks = 1-c(1, 0.927, 0.879, 0.782, 0.5, 0),
                     limits = c(0,1),
                     expand = c(0,0)
                     ) +
  ylab("Cumulative density of posterior with w=0.38") +
  xlab("Quantile") +
  geom_segment(aes(x = 0,
                   y = RBesT::pmix(mix = posterior_primary, q = 0), 
                   xend = 0, 
                   yend = 1), 
               col="red") +
  geom_segment(aes(x = 0.5,
                   y = RBesT::pmix(mix = posterior_primary, q = 0.5), 
                   xend = 0.5, 
                   yend = 1), 
               col="red") + 
  geom_segment(aes(x = 1,
                   y = RBesT::pmix(mix = posterior_primary, q = 1), 
                   xend = 1, 
                   yend = 1), 
               col="red") + 
  theme_bw()

## ---- eval=T, echo=T----------------------------------------------------------
tipp_points[3]

## ---- eval=T, echo=T----------------------------------------------------------
prior_95p <- RBesT::robustify(
  priormix = map_prior,
  weight = (1 - tipp_points[3]),
  m = 0,
  n = 1,
  sigma = uisd
  )

## ---- eval=T, echo=T----------------------------------------------------------
posterior_95p <- RBesT::postmix(
  priormix = prior_95p,
  m = pediatric_trial["mean"],
  se = pediatric_trial["se"]
  )

## ---- eval=T, echo=T----------------------------------------------------------
round(1 - RBesT::pmix(posterior_95p, q = 0), 3)

