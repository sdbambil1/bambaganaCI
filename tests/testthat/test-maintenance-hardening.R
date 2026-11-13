test_that("print methods reject invalid digits", {
  set.seed(1998)
  boot_result <- bootstrap_mean_ci(1:20, B = 200)
  t_result <- mean_t_ci(1:20)
  for (result in list(boot_result, t_result)) {
    for (bad in list(NA_real_, Inf, NaN, 0, -1, 2.5, 23, c(2, 3), "four")) {
      expect_error(print(result, digits = bad), "digits must")
    }
    expect_output(print(result, digits = 6), "Confidence interval")
  }
})

test_that("normal-only comparison does not give quantile tail warning", {
  set.seed(1998)
  expect_no_warning({
    out <- compare_mean_ci(1:20, B = 20, methods = "normal")
  })
  expect_equal(out$method, "normal")
  expect_true(is.finite(out$width))
})

test_that("quantile-based comparison warns on too few tail replicates", {
  set.seed(1998)
  expect_warning(
    compare_mean_ci(1:20, B = 20, methods = "percentile"),
    "interval tails"
  )
})
