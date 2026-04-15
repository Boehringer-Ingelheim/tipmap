test_that("oc_pos returns matrix-like result with expected dimensions", {
  map_prior <- load_tipmap_data("tipmapPrior.rds")
  m <- c(1.1, 1.2, 1.3)
  se <- c(1.5, 1.6, 1.7)
  weights <- c(0, 0.5, 1)
  probs <- c(0.025, 0.05, 0.1)
  
  x <- oc_pos(
    m = m,
    se = se,
    probs = probs,
    weights = weights,
    map_prior = map_prior,
    sigma = 16.23,
    null_effect = 0,
    direction_pos = TRUE,
    eval_strategy = "sequential",
    n_cores = 1
  )
  
  expect_equal(dim(x), c(length(weights), length(probs)))
  expect_equal(colnames(x), paste0("q=", probs))
})

test_that("oc_pos rejects invalid inputs", {
  map_prior <- load_tipmap_data("tipmapPrior.rds")
  expect_error(oc_pos(m = 1, se = 1, probs = c(0, 0.5), weights = 0, map_prior = map_prior, sigma = 1), "strictly between 0 and 1")
  expect_error(oc_pos(m = 1, se = 1, probs = 0.5, weights = 0, map_prior = map_prior, sigma = 1, direction_pos = NA), "TRUE or FALSE")
})