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
