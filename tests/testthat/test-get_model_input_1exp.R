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
