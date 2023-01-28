test_that("Getting cumulative probabilities works", {
  expect_equal(
    get_cum_probs_1exp(c(0, 0, 4, 2, 2, 1, 1, 0, 0, 0)),
    c(0.0, 0.0, 0.4, 0.6, 0.8, 0.9, 1.0, 1.0, 1.0, 1.0))
})
