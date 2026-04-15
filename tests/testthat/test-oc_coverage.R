test_that("oc_coverage returns matrix-like result with expected dimensions", {
  map_prior <- load_tipmap_data("tipmapPrior.rds")
  m <- c(1.1, 1.2, 1.3)
  se <- c(1.5, 1.6, 1.7)
  weights <- c(0, 0.5, 1)
  
  x <- oc_coverage(
    m = m,
    se = se,
    true_effect = 1.15,
    weights = weights,
    map_prior = map_prior,
    sigma = 16.23,
    eval_strategy = "sequential",
    n_cores = 1
  )
  
  expect_equal(dim(x), c(length(weights), 4))
  expect_equal(colnames(x), c("cov.50p", "cov.80p", "cov.90p", "cov.95p"))
})

test_that("oc_coverage rejects invalid inputs", {
  map_prior <- load_tipmap_data("tipmapPrior.rds")
  expect_error(oc_coverage(m = 1, se = 1, true_effect = 1, weights = -0.1, map_prior = map_prior, sigma = 1), "\\[0, 1\\]")
})