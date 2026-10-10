
test_that("extreme units preserve intervals and positive standard errors", {
  x <- 1:10

  for (method in c("percentile", "basic", "normal", "bca")) {
    set.seed(1998)
    reference <- bootstrap_mean_ci(x, method = method)

    for (scale in c(1e-200, 1e200)) {
      set.seed(1998)
      result <- bootstrap_mean_ci(x * scale, method = method)

      expect_true(result$bootstrap_se > 0)
      expect_true(result$lower < result$upper)

      expect_equal(
        c(result$estimate, result$lower, result$upper,
          result$bootstrap_se) / scale,
        c(reference$estimate, reference$lower, reference$upper,
          reference$bootstrap_se),
        tolerance = 1e-10
      )
    }
  }
})

