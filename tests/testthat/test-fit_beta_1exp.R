test_that("Beta fit for 1 expert works", {
  expect_equal(round(as.numeric(
    fit_beta_1exp(
      c(0, 0, 3, 2, 1, 2, 1, 1, 0, 0) %>%
        get_cum_probs_1exp() %>%
        get_model_input_1exp()
    )$par
  ), 6),
  c(2.088584, 2.816686))
})

# note:
# - "round" is because the result had a lot of decimals
# - "as.numeric" is because the first input has names and the second does not
test_that("fit_beta_1exp returns expected components", {
  chips <- c(0, 2, 3, 2, 1, 1, 1, 0, 0, 0)
  x <- get_cum_probs_1exp(chips)
  y <- get_model_input_1exp(x)
  fit <- fit_beta_1exp(df = y)
  
  expect_true(is.list(fit))
  expect_true(all(c("par", "value", "counts", "convergence") %in% names(fit)))
  expect_true(all(fit$par > 0))
})

test_that("fit_beta_1exp rejects invalid inputs", {
  expect_error(fit_beta_1exp(list(w = 1:3, cum_probs = c(0.2, 0.5, 1))), "data frame")
  expect_error(fit_beta_1exp(data.frame(a = 1:3, b = c(0.2, 0.5, 1))), "exactly")
  expect_error(fit_beta_1exp(data.frame(w = 1:3, cum_probs = c(0, 1, 1))), "at least two")
})