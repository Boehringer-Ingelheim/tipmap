test_that("Getting model input works", {
  expect_equal(nrow(
    c(0, 1, 3, 3, 2, 1, 0, 0, 0, 0) %>% get_cum_probs_1exp() %>%
      get_model_input_1exp()
  ),
  5)
  expect_equal(ncol(
    c(0, 1, 3, 3, 2, 1, 0, 0, 0, 0) %>% get_cum_probs_1exp() %>%
      get_model_input_1exp()
  ),
  2)
  expect_equal((
    c(0, 1, 3, 3, 2, 1, 0, 0, 0, 0) %>% get_cum_probs_1exp() %>%
      get_model_input_1exp()
  )$w,
  c(0.2, 0.3, 0.4, 0.5, 0.6))
  expect_equal(
    (
      c(0, 1, 3, 3, 2, 1, 0, 0, 0, 0) %>% get_cum_probs_1exp() %>%
        get_model_input_1exp()
    )$cum_probs,
    c(0.1, 0.4, 0.7, 0.9, 1.0)
  )
})
test_that("get_model_input_1exp returns expected data frame", {
  cum_probs <- c(0, 0.2, 0.5, 0.5, 1)
  x <- get_model_input_1exp(cum_probs)
  
  expect_s3_class(x, "data.frame")
  expect_equal(names(x), c("w", "cum_probs"))
  expect_true(all(x$cum_probs > 0))
  expect_equal(x$cum_probs, c(0.2, 0.5, 1))
})

test_that("get_model_input_1exp honors custom weights", {
  cum_probs <- c(0.2, 0.5, 1)
  w <- c(0.25, 0.5, 1)
  x <- get_model_input_1exp(cum_probs, w = w)
  
  expect_equal(x$w, w)
})

test_that("get_model_input_1exp rejects invalid inputs", {
  expect_error(get_model_input_1exp(c(0.2, 0.1, 1)), "non-decreasing")
  expect_error(get_model_input_1exp(c(-0.1, 0.2, 1)), "\\[0, 1\\]")
  expect_error(get_model_input_1exp(c(0.2, 0.5, 1), w = c(0.5, 1)), "same length")
})