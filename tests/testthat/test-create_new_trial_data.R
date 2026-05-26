test_that("return new trial data", {
  expect_equal(unname(create_new_trial_data(
    n_total = 30,
    est = 1.27,
    se = 0.95
  )),
  c(30, 1.27, 0.95,
    qnorm(p = default_quantiles, mean = 1.27, sd = 0.95)
  ))
})

test_that("create_new_trial_data returns expected structure", {
  x <- create_new_trial_data(n_total = 30, est = 1.27, se = 0.95)
  
  expect_type(x, "double")
  expect_named(x)
  expect_equal(names(x), c("n_total", "mean", "se", paste0("q", default_quantiles)))
  expect_length(x, 3 + length(default_quantiles))
  expect_equal(unname(x[c("n_total", "mean", "se")]), c(30, 1.27, 0.95))
})

test_that("create_new_trial_data rejects invalid inputs", {
  expect_error(create_new_trial_data("30", 1.27, 0.95), "n_total")
  expect_error(create_new_trial_data(c(30, 31), 1.27, 0.95), "length 1")
  expect_error(create_new_trial_data(30.5, 1.27, 0.95), "whole number")
  expect_error(create_new_trial_data(0, 1.27, 0.95), "positive")
  expect_error(create_new_trial_data(30, "a", 0.95), "est")
  expect_error(create_new_trial_data(30, c(1, 2), 0.95), "est")
  expect_error(create_new_trial_data(30, 1.27, 0), "positive")
  expect_error(create_new_trial_data(30, 1.27, NA_real_), "finite")
})
