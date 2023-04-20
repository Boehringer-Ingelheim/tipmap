
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tipmap

<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![](https://www.r-pkg.org/badges/version/tipmap)](https://cran.r-project.org/package=tipmap)
[![](http://cranlogs.r-pkg.org/badges/last-month/tipmap)](https://cran.r-project.org/package=tipmap)
[![](http://cranlogs.r-pkg.org/badges/grand-total/tipmap)](https://cran.r-project.org/package=tipmap)
<!-- badges: end -->

The `tipmap` package aims to facilitate the planning and analysis of
partial extrapolation studies in pediatric drug development. It provides
an implementation of a Bayesian tipping point approach based on robust
meta-analytic predictive (MAP) priors, with further functions
facilitating expert elicitation of a primary weight of the informative
component of the prior.

## Installation

**CRAN**

You can install the current stable version from CRAN with:

``` r
install.packages("tipmap")
```

**GitHub**

You can install the current development version from GitHub with:

``` r
if (!require("remotes")) {install.packages("remotes")}
remotes::install_github("chstock/tipmap")
```

## Getting started

Load the package:

``` r
library(tipmap)
```

The prior data (collected in the source population):

``` r
prior_data <- create_prior_data(
  n_total = c(160, 240, 320),
  est = c(1.23, 1.40, 1.51),
  se = c(0.4, 0.36, 0.31)
)
print(prior_data)
```

The data from the new trial (collected in the target population):

``` r
ped_trial <- create_new_trial_data(
  n_total = 30, 
  est = 1.27, 
  se = 0.95
)
print(ped_trial)
```

Derivation of the meta-analytic predictive (MAP) prior:

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
print(map_prior)
```

Computing the posterior distribution:

``` r
posterior <- create_posterior_data(
  map_prior = map_prior,
  new_trial_data = ped_trial,
  sigma = uisd)
print(posterior)
```

Create data for a tipping point analysis (tipping point plot):

``` r
tipmap_data <- create_tipmap_data(
  new_trial_data = ped_trial,
  posterior = posterior,
  map_prior = map_prior)
```

Create tipping point plot:

``` r
tipmap_plot(tipmap_data = tipmap_data)
```

Get tipping points:

``` r
get_tipping_points(
  tipmap_data, 
  quantile = c(0.025, 0.05, 0.1, 0.2), 
  null_effect = 0.1)
```

## Citing `tipmap`

To cite `tipmap` in publications please use: Morten Dreher and Christian
Stock (2022). tipmap: Tipping Point Analysis for Bayesian Dynamic
Borrowing. R package version 0.3.9. URL:
<https://CRAN.R-project.org/package=tipmap>
