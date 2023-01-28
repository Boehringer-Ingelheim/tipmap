# tipmap

The `tipmap` package aims to facilitate the planning and analysis of partial extrapolation studies in pediatric drug development. It provides an implementation of a Bayesian tipping point approach based on robust meta-analytic predictive (MAP) priors, with further functions facilitating expert elicitation of a primary weight of the informative component of the prior.

## Installation

To install `tipmap` use:

``` r
install.packages("tipmap")
```

## Example


``` r
library(tipmap)
ped_trial <- create_new_trial_data(
  n_total = 30, 
  est = 1.27, 
  se = 0.95
)
ped_trial
```

``` r
prior_data <- create_prior_data(
  n_total = c(160, 240, 320),
  est = c(1.23, 1.40, 1.51),
  se = c(0.4, 0.36, 0.31)
)
prior_data
```

``` r
uisd <- sqrt(ped_trial["n_total"]) * ped_trial["se"]
g_map <-
  RBesT::gMAP(
    formula = cbind(est, se) ~ 1 | study_label,
    data = prior_data,
    family = gaussian,
    weights = n_total,
    tau.dist = "HalfNormal",
    tau.prior = cbind(0, uisd / 16),
    beta.prior = cbind(0, uisd)
  )
```

``` r
map_prior <-
  RBesT::automixfit(
    sample = g_map,
    Nc = seq(1, 4),
    k = 6,
    thresh = -Inf
  )
map_prior
```

``` r
posterior <- create_posterior_data(
  map_prior = map_prior,
  new_trial_data = ped_trial,
  sigma = uisd)
posterior
```

``` r
tipmap_data <- create_tipmap_data(
  new_trial_data = ped_trial,
  posterior = posterior,
  map_prior = map_prior)
```

``` r
tipmap_plot(tipmap_data = tipmap_data)
```

``` r
get_tipping_points(
  tipmap_data, 
  quantile = c(0.025, 0.05, 0.1, 0.2), 
  null_effect = 0.1)
```
