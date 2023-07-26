set.seed(42)
test_that("Get summary for multiple expert works", {
  expect_equal(
    round(
      as.numeric(
        get_summary_mult_exp(
          chips_mult = rbind(
            c(0, 0, 3, 2, 1, 2, 1, 1, 0, 0),
            c(0, 0, 4, 2, 2, 1, 1, 0, 0, 0),
            c(0, 0, 5, 3, 2, 0, 0, 0, 0, 0)
            ), 
          n = 10,
          expert_weight = rep(1/3, 3)
        )
      ),
      5
    ),
    c(0.12794, 0.28430, 0.45499, 0.46382, 0.57424, 0.89817)
  )
})
