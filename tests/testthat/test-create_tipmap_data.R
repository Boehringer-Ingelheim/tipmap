test_that("return tipping point data", {
  tip_dat <- create_tipmap_data(
    new_trial_data = create_new_trial_data(30, 1.4, 2.0),
    posterior = load_tipmap_data("tipPost.rds"),
    map_prior = load_tipmap_data("tipmapPrior.rds")
  )
  expect_equal(tip_dat, load_tipmap_data("tipdat.rds"))
})

test_that("create_tipmap_data works without meta-analysis", {
  new_trial_data <- create_new_trial_data(n_total = 30, est = 1.5, se = 2.1)
  map_prior <- load_tipmap_data("tipmapPrior.rds")
  posterior <- load_tipmap_data("tipPost.rds")
  
  x <- create_tipmap_data(
    new_trial_data = new_trial_data,
    posterior = posterior,
    map_prior = map_prior
  )
  
  expect_s3_class(x, "data.frame")
  expect_true(all(c("x.at", "x.col", "t.est", "t.0.025", "t.0.975") %in% names(x)))
  expect_equal(nrow(x), length(default_weights) + 2)
})

test_that("create_tipmap_data works with meta-analysis", {
  new_trial_data <- create_new_trial_data(n_total = 30, est = 1.5, se = 2.1)
  map_prior <- load_tipmap_data("tipmapPrior.rds")
  posterior <- load_tipmap_data("tipPost.rds")
  meta_analysis <- data.frame(TE.fixed = 1.1, lower.fixed = 0.2, upper.fixed = 2.0)
  
  x <- create_tipmap_data(
    new_trial_data = new_trial_data,
    posterior = posterior,
    map_prior = map_prior,
    meta_analysis = meta_analysis
  )
  
  expect_equal(nrow(x), length(default_weights) + 3)
})

test_that("create_tipmap_data rejects invalid inputs", {
  new_trial_data <- create_new_trial_data(n_total = 30, est = 1.5, se = 2.1)
  map_prior <- load_tipmap_data("tipmapPrior.rds")
  posterior <- load_tipmap_data("tipPost.rds")
  
  expect_error(create_tipmap_data(new_trial_data, posterior = 1, map_prior = map_prior), "posterior")
  expect_error(create_tipmap_data(c(mean = 1), posterior = posterior, map_prior = map_prior), "new_trial_data")
  expect_error(
    create_tipmap_data(
      new_trial_data = new_trial_data,
      posterior = posterior,
      map_prior = map_prior,
      meta_analysis = data.frame(a = 1)
    ),
    "meta_analysis"
  )
})