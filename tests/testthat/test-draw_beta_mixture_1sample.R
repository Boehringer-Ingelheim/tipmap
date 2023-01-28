set.seed(42)
test_that("Draw Beta mixture for 1 sample works", {
  expect_equal(
    round(fit_beta_mult_exp(
      chips_mult =
        rbind(
          c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
          c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
          c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
        )
    ) %>% draw_beta_mixture_1sample(),7),
    0.3659566
  )
})
