test_that("draw_beta_mixture_nsamples returns values in [0, 1]", {
  chips_mult <- rbind(
    c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
    c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
    c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
  )
  
  set.seed(123)
  x <- draw_beta_mixture_nsamples(n = 50, chips_mult = chips_mult)
  
  expect_length(x, 50)
  expect_true(is.numeric(x))
  expect_true(all(x >= 0 & x <= 1))
})

test_that("draw_beta_mixture_nsamples is reproducible under set.seed", {
  chips_mult <- rbind(
    c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
    c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
    c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
  )
  
  set.seed(1)
  x1 <- draw_beta_mixture_nsamples(n = 10, chips_mult = chips_mult)
  
  set.seed(1)
  x2 <- draw_beta_mixture_nsamples(n = 10, chips_mult = chips_mult)
  
  expect_equal(x1, x2)
})

test_that("draw_beta_mixture_nsamples rejects invalid expert weights", {
  chips_mult <- rbind(
    c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
    c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0)
  )
  
  expect_error(
    draw_beta_mixture_nsamples(10, chips_mult, expert_weight = c(1, 1, 1)),
    "one entry per expert"
  )
  
  expect_error(
    draw_beta_mixture_nsamples(10, chips_mult, expert_weight = c(1, -1)),
    "non-negative"
  )
})