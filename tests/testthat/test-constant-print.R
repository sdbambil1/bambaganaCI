test_that("constant data prints an uncertainty warning", {
  set.seed(1998)

  result <- bootstrap_mean_ci(
    rep(18, 30),
    B = 2000,
    method = "percentile"
  )

  expect_equal(result$lower, 18)
  expect_equal(result$upper, 18)

  output <- capture.output(print(result))

  expect_true(any(grepl(
    "zero width does not imply zero population uncertainty",
    output,
    fixed = TRUE
  )))
})
