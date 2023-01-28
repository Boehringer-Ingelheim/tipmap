test_that("return prior data", {
  prior <- create_prior_data(
    n_total = c(200, 250, 190),
    est = c(1.6, 1.55, 1.4),
    se = c(0.7, 0.5, 0.9)
  )
  expect_equal(prior, data.frame(
    study_label = c("Study 1", "Study 2", "Study 3"),
    n_total = c(200, 250, 190),
    est = c(1.6, 1.55, 1.4),
    se = c(0.7, 0.5, 0.9)
  ))
})
