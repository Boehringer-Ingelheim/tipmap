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
test_that("load_tipmap_data loads example datasets", {
  tipdat <- load_tipmap_data("tipdat.rds")
  tipmap_prior <- load_tipmap_data("tipmapPrior.rds")
  tip_post <- load_tipmap_data("tipPost.rds")
  
  expect_s3_class(tipdat, "data.frame")
  expect_true("normMix" %in% class(tipmap_prior))
  expect_s3_class(tip_post, "data.frame")
})

test_that("load_tipmap_data rejects invalid file names", {
  expect_error(load_tipmap_data("does_not_exist.rds"), "Could not find")
  expect_error(load_tipmap_data(1), "character")
})