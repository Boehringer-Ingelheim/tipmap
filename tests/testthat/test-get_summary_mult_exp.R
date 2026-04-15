test_that("get_summary_mult_exp returns summary statistics", {
  chips_mult <- rbind(
    c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
    c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
    c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
  )
  
  set.seed(123)
  x <- get_summary_mult_exp(chips_mult = chips_mult, n = 50)
  
  expect_named(x, c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."))
  expect_true(all(is.finite(x)))
  expect_true(x["Min."] >= 0)
  expect_true(x["Max."] <= 1)
})

test_that("get_summary_mult_exp rejects invalid inputs", {
  expect_error(
    get_summary_mult_exp(chips_mult = matrix("a", nrow = 1), n = 50),
    "numeric"
  )
  
  expect_error(
    get_summary_mult_exp(chips_mult = matrix(1:4, nrow = 2), n = 0),
    "positive"
  )
})

test_that("get_summary_mult_exp works with custom expert weights", {
  chips_mult <- rbind(
    c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
    c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
    c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
  )
  
  set.seed(123)
  x <- get_summary_mult_exp(
    chips_mult = chips_mult,
    n = 50,
    expert_weight = c(0.2, 0.3, 0.5)
  )
  
  expect_named(x, c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."))
  expect_true(all(is.finite(x)))
})