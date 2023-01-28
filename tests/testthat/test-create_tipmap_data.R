test_that("return tipping point data", {
  tip_dat <- create_tipmap_data(
    new_trial_data = create_new_trial_data(30, 1.4, 2.0),
    posterior = load_tipmap_data("tipPost.rds"),
    map_prior = load_tipmap_data("tipmapPrior.rds")
  )
  expect_equal(tip_dat, load_tipmap_data("tipdat.rds"))
})
