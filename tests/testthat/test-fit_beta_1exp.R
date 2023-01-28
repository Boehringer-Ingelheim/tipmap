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
