test_that("return posterior data", {
  new_trial <- create_new_trial_data(30, 1.4, 2.0)
  post <- create_posterior_data(
    map_prior = load_tipmap_data("tipmapPrior.rds"),
    new_trial_data = new_trial, sigma = 12
  )
  expect_equal(post, load_tipmap_data("tipPost.rds"), tolerance = 1e-3)
})
