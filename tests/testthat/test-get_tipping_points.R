test_that("return tipping point", {
  expect_equal(
    unname(get_tipping_points(
      tipmap_data = load_tipmap_data("tipdat.rds"),
      quantile = c(0.025, 0.05, 0.1, 0.2)
    )),
    c(0.750, 0.475, 0.220, 0.040)
  )
})
test_that("get_tipping_points returns named numeric vector", {
  tip_dat <- load_tipmap_data("tipdat.rds")
  x <- get_tipping_points(tip_dat, quantile = c(0.025, 0.05, 0.1, 0.2))
  
  expect_type(x, "double")
  expect_named(x, paste0("q", c(0.025, 0.05, 0.1, 0.2)))
  expect_length(x, 4)
})

test_that("get_tipping_points rejects invalid inputs", {
  tip_dat <- load_tipmap_data("tipdat.rds")
  expect_error(get_tipping_points(tip_dat, quantile = 0.5), "quantile")
  expect_error(get_tipping_points(1, quantile = 0.025), "tipmap_data")
  expect_error(get_tipping_points(tip_dat, quantile = 0.025, null_effect = c(0, 1)), "length 1")
})
test_that("get_tipping_points returns NA when tipping point is >= 1", {
  tipmap_data <- data.frame(
    x.at = c(0, 0.5, 1.15),
    t.0.025 = c(2, 1, 0.01)
  )
  
  expect_message(
    x <- get_tipping_points(tipmap_data, quantile = 0.025, null_effect = 0),
    "Weight >=1 identified"
  )
  
  expect_true(is.na(x[[1]]))
})

test_that("get_tipping_points reports weight 0 tipping point", {
  tipmap_data <- data.frame(
    x.at = c(0, 0.5, 0.8),
    t.0.025 = c(0.01, 0.5, 1)
  )
  
  expect_message(
    get_tipping_points(tipmap_data, quantile = 0.025, null_effect = 0),
    "Weight 0 identified"
  )
})