test_that("return posterior data", {
  new_trial <- create_new_trial_data(30, 1.4, 2.0)
  post <- create_posterior_data(
    map_prior = load_tipmap_data("tipmapPrior.rds"),
    new_trial_data = new_trial, sigma = 12
  )
  expect_equal(post, load_tipmap_data("tipPost.rds"), tolerance = 1e-3)
})

test_that("create_posterior_data returns expected shape for equal-tailed intervals", {
  new_trial_data <- create_new_trial_data(n_total = 30, est = 1.27, se = 0.95)
  map_prior <- load_tipmap_data("tipmapPrior.rds")
  
  x <- create_posterior_data(
    map_prior = map_prior,
    new_trial_data = new_trial_data,
    sigma = 12,
    interval_type = "equal-tailed"
  )
  
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), length(default_weights))
  expect_equal(names(x), c("weight", paste0("q", default_quantiles)))
  expect_equal(x$weight, default_weights)
})

test_that("create_posterior_data returns expected shape for hpdi intervals", {
  set.seed(123)
  
  new_trial_data <- create_new_trial_data(
    n_total = 30,
    est = 1.27,
    se = 0.95
  )
  map_prior <- load_tipmap_data("tipmapPrior.rds")
  
  x <- create_posterior_data(
    map_prior = map_prior,
    new_trial_data = new_trial_data,
    sigma = 12,
    interval_type = "hpdi",
    n_samples = 250
  )
  
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), length(default_weights))
  expect_equal(names(x), c("weight", paste0("q", default_quantiles)))
  expect_equal(x$weight, default_weights)
  
  expect_true(all(is.finite(x$q0.5)))
  expect_true(all(x$q0.025 <= x$q0.5))
  expect_true(all(x$q0.5 <= x$q0.975))
})

test_that("create_posterior_data rejects invalid inputs", {
  new_trial_data <- create_new_trial_data(n_total = 30, est = 1.27, se = 0.95)
  map_prior <- load_tipmap_data("tipmapPrior.rds")
  
  expect_error(create_posterior_data(map_prior, new_trial_data, sigma = 0), "positive")
  expect_error(create_posterior_data(map_prior, new_trial_data, sigma = 12, interval_type = "bad"), "interval_type")
  expect_error(create_posterior_data(map_prior, new_trial_data, sigma = 12, interval_type = "hpdi", n_samples = 0), "positive")
  expect_error(create_posterior_data(map_prior, c(mean = 1.2), sigma = 12), "new_trial_data")
})

test_that("create_posterior_data rejects invalid map_prior", {
  new_trial_data <- create_new_trial_data(n_total = 30, est = 1.27, se = 0.95)
  
  expect_error(
    create_posterior_data(
      map_prior = 1,
      new_trial_data = new_trial_data,
      sigma = 12
    ),
    "map_prior"
  )
})

test_that("create_posterior_data rejects invalid null_effect", {
  new_trial_data <- create_new_trial_data(n_total = 30, est = 1.27, se = 0.95)
  map_prior <- load_tipmap_data("tipmapPrior.rds")
  
  expect_error(
    create_posterior_data(
      map_prior = map_prior,
      new_trial_data = new_trial_data,
      sigma = 12,
      null_effect = c(0, 1)
    ),
    "null_effect"
  )
})

