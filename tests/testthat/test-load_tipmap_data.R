test_that("load MAP prior data", {
  expect_type(load_tipmap_data("tipmapPrior.rds"), "double")
  expect_length(load_tipmap_data("tipmapPrior.rds"), 9)
})

test_that("load posterior data", {
  expect_type(load_tipmap_data("tipPost.rds"), "list")
  expect_length(load_tipmap_data("tipPost.rds"), 14)
})

test_that("load tipmap data", {
  expect_type(load_tipmap_data("tipdat.rds"), "list")
  expect_length(load_tipmap_data("tipdat.rds"), 11)
})
