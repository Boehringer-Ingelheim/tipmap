test_that("oc_bias returns matrix-like result with expected dimensions", {
  map_prior <- load_tipmap_data("tipmapPrior.rds")
  m <- c(1.1, 1.2, 1.3)
  se <- c(1.5, 1.6, 1.7)
  weights <- c(0, 0.5, 1)
  
  x <- oc_bias(
    m = m,
    se = se,
    true_effect = 1.15,
    weights = weights,
    map_prior = map_prior,
    sigma = 16.23,
    eval_strategy = "sequential",
    n_cores = 1
  )
  
  expect_equal(dim(x), c(length(weights), 2))
  expect_equal(colnames(x), c("abs.bias.post.mean", "abs.bias.post.median"))
})

test_that("oc_bias rejects invalid inputs", {
  map_prior <- load_tipmap_data("tipmapPrior.rds")
  expect_error(oc_bias(m = 1:2, se = 1, true_effect = 1, weights = 0, map_prior = map_prior, sigma = 1), "same length")
  expect_error(oc_bias(m = 1, se = -1, true_effect = 1, weights = 0, map_prior = map_prior, sigma = 1), "positive")
})