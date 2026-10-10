test_that("bootstrap methods are reproducible", {
  x <- c(12, 15, 18, 21, 25, 27, 30, 34, 36, 40)
  methods <- c("percentile", "basic", "normal", "bca")
  for (method in methods) {
    set.seed(1998)
    first <- bootstrap_mean_ci(x, B = 2000, method = method)
    set.seed(1998)
    second <- bootstrap_mean_ci(x, B = 2000, method = method)
    expect_identical(first, second)
  }
})

test_that("bootstrap results support S3 extraction", {
  x <- c(12, 15, 18, 21, 25, 27, 30, 34, 36, 40)
  for (method in c("percentile", "basic", "normal", "bca")) {
    set.seed(1998)
    result <- bootstrap_mean_ci(x, B = 2000, method = method)
    expect_equal(unname(coef(result)), result$estimate)
    expect_equal(as.numeric(confint(result)),
                 c(result$lower, result$upper))
    expect_s3_class(as.data.frame(result), "data.frame")
    expect_equal(nrow(as.data.frame(result)), 1L)
  }
})

test_that("constant samples are handled consistently", {
  x <- rep(10, 30)
  for (method in c("percentile", "basic", "normal", "bca")) {
    set.seed(1998)
    result <- bootstrap_mean_ci(x, B = 200, method = method)
    expect_equal(result$lower, 10)
    expect_equal(result$upper, 10)
    expect_true(result$diagnostics$constant_data)
  }
  expect_error(mean_t_ci(x), "constant sample")
})
