test_that("Getting cumulative probabilities works", {
  expect_equal(
    get_cum_probs_1exp(c(0, 0, 4, 2, 2, 1, 1, 0, 0, 0)),
    c(0.0, 0.0, 0.4, 0.6, 0.8, 0.9, 1.0, 1.0, 1.0, 1.0))
})

test_that("get_cum_probs_1exp returns cumulative probabilities", {
  chips <- c(0, 2, 3, 2, 1, 1, 1, 0, 0, 0)
  x <- get_cum_probs_1exp(chips)
  
  expect_type(x, "double")
  expect_equal(tail(x, 1), 1)
  expect_true(all(diff(x) >= 0))
})

test_that("get_cum_probs_1exp rejects invalid inputs", {
  expect_error(get_cum_probs_1exp(c(-1, 1, 2)), "non-negative")
  expect_error(get_cum_probs_1exp(c(0.5, 1, 2)), "whole")
  expect_error(get_cum_probs_1exp(c(0, 0, 0)), "positive")
  expect_error(get_cum_probs_1exp(c(1, NA)), "finite")
})