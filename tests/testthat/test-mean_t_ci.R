
test_that("t intervals agree with one-sample and OLS references", {
  set.seed(1998)
  x <- rnorm(30, 2, 1)

  for (level in c(0.90, 0.95, 0.99)) {
    result <- mean_t_ci(x, conf = level)
    fit <- stats::lm(x ~ 1)

    expect_equal(
      result$estimate,
      unname(stats::coef(fit)),
      tolerance = 1e-12
    )

    expect_equal(
      c(result$lower, result$upper),
      as.numeric(stats::t.test(x, conf.level = level)$conf.int),
      tolerance = 1e-12
    )

    expect_equal(
      c(result$lower, result$upper),
      as.numeric(stats::confint(fit, level = level)),
      tolerance = 1e-12
    )
  }
})

test_that("t intervals preserve extreme measurement scales", {
  set.seed(1998)
  x <- rnorm(30, 2, 1)
  reference <- mean_t_ci(x)

  for (scale in c(1e-200, 1e200)) {
    result <- mean_t_ci(x * scale)

    expect_equal(
      c(result$estimate, result$lower, result$upper,
        result$standard_error) / scale,
      c(reference$estimate, reference$lower, reference$upper,
        reference$standard_error),
      tolerance = 1e-10
    )
  }
})

test_that("t intervals reject invalid or degenerate input", {
  expect_error(mean_t_ci(rep(5, 10)), "constant sample")
  expect_error(mean_t_ci(c(1, NA)), "finite numeric")
  expect_error(mean_t_ci(c(1, Inf)), "finite numeric")
  expect_error(mean_t_ci(matrix(1:6, 2)), "finite numeric")
  expect_error(mean_t_ci(1), "finite numeric")
  expect_error(mean_t_ci(1:5, conf = 95), "strictly between")
  expect_error(mean_t_ci(1:5, conf = NA_real_), "strictly between")
})

