set.seed(42)
test_that("Draw Beta mixture for n samples works", {
  expect_equal(
    round(draw_beta_mixture_nsamples(
      n = 10,
      chips_mult = rbind(
        c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
        c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
        c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
      )),7),
    c(0.3659566, 0.5476925, 0.6845267, 0.4273690, 0.7504038, 0.3777367,
      0.3518233, 0.4021988, 0.5084292, 0.3465878)
  )
})
