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

test_that("create_prior_data creates default study labels", {
  x <- create_prior_data(
    n_total = c(160, 240, 320),
    est = c(1.23, 1.40, 1.51),
    se = c(0.4, 0.36, 0.31)
  )
  
  expect_s3_class(x, "data.frame")
  expect_equal(x$study_label, c("Study 1", "Study 2", "Study 3"))
  expect_equal(names(x), c("study_label", "n_total", "est", "se"))
})

test_that("create_prior_data keeps user study labels", {
  x <- create_prior_data(
    study_label = c("A", "B"),
    n_total = c(100, 120),
    est = c(1.1, 1.2),
    se = c(0.2, 0.3)
  )
  
  expect_equal(x$study_label, c("A", "B"))
})

test_that("create_prior_data rejects invalid inputs", {
  expect_error(create_prior_data(n_total = c(100, 101.5), est = c(1, 2), se = c(0.2, 0.3)), "whole")
  expect_error(create_prior_data(n_total = c(100, -1), est = c(1, 2), se = c(0.2, 0.3)), "positive")
  expect_error(create_prior_data(n_total = c(100, 101), est = c(1, 2, 3), se = c(0.2, 0.3)), "same length")
  expect_error(create_prior_data(study_label = c("A"), n_total = c(100, 101), est = c(1, 2), se = c(0.2, 0.3)), "study_label")
  expect_error(create_prior_data(n_total = c(100, 101), est = c(1, 2), se = c(0.2, 0)), "positive")
})