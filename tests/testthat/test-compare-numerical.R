test_that("comparison intervals are numerically valid", {
  cases <- list(
    Large = c(1e150, 2e150, 3e150, 4e150),
    Small = c(1e-150, 2e-150, 3e-150, 4e-150),
    Large_offset = 1e15 + 0:4,
    Near_constant = 1e10 + (0:4) * 1e-4,
    Ordinary = c(2, 3, 4, 5, 6)
  )
  for (x in cases) {
    set.seed(1998)
    out <- compare_mean_ci(x, B = 2000)
    expect_true(all(is.finite(out$lower)))
    expect_true(all(is.finite(out$upper)))
    expect_true(all(is.finite(out$width)))
    expect_true(all(out$width > 0))
    expect_true(all(out$lower < out$upper))
  }
})

test_that("BCa and t agree with existing functions", {
  cases <- list(
    c(1e150, 2e150, 3e150, 4e150),
    c(1e-150, 2e-150, 3e-150, 4e-150),
    1e15 + 0:4,
    1e10 + (0:4) * 1e-4,
    c(2, 3, 4, 5, 6)
  )
  for (x in cases) {
    for (method in c("bca", "t")) {
      set.seed(1998)
      expected <- if (method == "t") {
        mean_t_ci(x)
      } else {
        bootstrap_mean_ci(x, B = 2000, method = "bca")
      }
      set.seed(1998)
      actual <- compare_mean_ci(x, B = 2000, methods = method)
      expect_equal(actual$lower, expected$lower, tolerance = 1e-10)
      expect_equal(actual$upper, expected$upper, tolerance = 1e-10)
    }
  }
})

test_that("BCa respects positive scaling", {
  x <- c(1, 2, 3, 4)
  set.seed(1998)
  baseline <- compare_mean_ci(x, B = 2000, methods = "bca")
  set.seed(1998)
  scaled <- compare_mean_ci(x * 1e150, B = 2000, methods = "bca")
  expect_equal(scaled$lower / 1e150, baseline$lower,
               tolerance = 1e-10)
  expect_equal(scaled$upper / 1e150, baseline$upper,
               tolerance = 1e-10)
})
