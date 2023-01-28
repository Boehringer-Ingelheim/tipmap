set.seed(42)
test_that("Get summary for multiple expert works", {
  expect_equal(
    round(as.numeric(get_summary_mult_exp(rbind(
      c(0, 0, 3, 2, 1, 2, 1, 1, 0, 0),
      c(0, 0, 4, 2, 2, 1, 1, 0, 0, 0),
      c(0, 0, 5, 3, 2, 0, 0, 0, 0, 0)))),6),
    c(0.004577, 0.228784, 0.331532, 0.352057, 0.451861, 0.898171)
  )
})
