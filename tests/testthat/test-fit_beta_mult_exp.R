test_that("fit_beta_mult_exp returns one row per expert", {
  chips_mult <- rbind(
    c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
    c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
    c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
  )
  
  x <- fit_beta_mult_exp(chips_mult)
  
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 3)
  expect_equal(names(x), c("alpha", "beta", "error", "convergence"))
  expect_true(all(x$alpha > 0))
  expect_true(all(x$beta > 0))
})

test_that("fit_beta_mult_exp rejects invalid inputs", {
  expect_error(fit_beta_mult_exp(matrix(c(-1, 1, 2, 3), nrow = 1)), "non-negative")
  expect_error(fit_beta_mult_exp(matrix(c(0.5, 1, 2, 3), nrow = 1)), "whole")
  expect_error(fit_beta_mult_exp(matrix(0, nrow = 2, ncol = 3)), "at least one chip")
})