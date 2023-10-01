set.seed(42)
test_that("Draw samples from beta mixture works", {
  expect_equal(
    round(draw_beta_mixture_nsamples(
      n = 10,
      chips_mult = rbind(
        c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
        c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
        c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
      )), 5),
    c(0.36596, 0.54769, 0.74003, 0.61173, 0.68343,
      0.28172, 0.51621, 0.72015, 0.27481, 0.50843)
  )
})

