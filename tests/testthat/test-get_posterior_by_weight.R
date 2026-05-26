test_that("get_posterior_by_weight filters rows correctly", {
  posterior <- load_tipmap_data("tipPost.rds")
  x <- get_posterior_by_weight(posterior, weight = c(0.05, 0.1))
  
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 2)
  expect_false("weight" %in% names(x))
})

test_that("get_posterior_by_weight returns zero rows for absent weights", {
  posterior <- load_tipmap_data("tipPost.rds")
  x <- get_posterior_by_weight(posterior, weight = 99)
  
  expect_equal(nrow(x), 0)
})

test_that("get_posterior_by_weight rejects invalid inputs", {
  posterior <- load_tipmap_data("tipPost.rds")
  expect_error(get_posterior_by_weight(1, 0.1), "posterior")
  expect_error(get_posterior_by_weight(posterior, "0.1"), "weight")
})