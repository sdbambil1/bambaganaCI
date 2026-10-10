test_that("compare_mean_ci rejects invalid observations", {
  expect_error(compare_mean_ci(c(1, NA, 3)),
               "finite numeric observations")
  expect_error(compare_mean_ci(1),
               "at least two")
})

test_that("compare_mean_ci rejects invalid B", {
  expect_error(compare_mean_ci(1:10, B = 1),
               "whole number")
  expect_error(compare_mean_ci(1:10, B = 2.5),
               "whole number")
})

test_that("compare_mean_ci rejects invalid confidence levels", {
  expect_error(compare_mean_ci(1:10, conf = 0),
               "strictly between")
  expect_error(compare_mean_ci(1:10, conf = 1),
               "strictly between")
})

test_that("BCa requires sufficient observations", {
  expect_error(
    compare_mean_ci(c(1, 2), methods = "bca"),
    "at least three"
  )
})

test_that("small B produces a tail-resolution warning", {
  expect_warning(
    compare_mean_ci(
      1:10, B = 10, methods = "percentile"
    ),
    "Too few resamples"
  )
})
