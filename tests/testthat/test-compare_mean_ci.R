test_that("comparison returns five methods", {
  set.seed(1998)
  x <- rnorm(30)
  out <- compare_mean_ci(x, B = 2000)
  expect_equal(nrow(out), 5L)
  expect_identical(out$method,
    c("percentile", "basic", "normal", "bca", "t"))
  expect_true(all(is.finite(out$lower)))
  expect_true(all(is.finite(out$upper)))
  expect_true(all(out$lower <= out$upper))
  expect_equal(out$width, out$upper - out$lower)
})

test_that("comparison supports method selection", {
  set.seed(1998)
  out <- compare_mean_ci(1:20, B = 2000,
    methods = c("percentile", "t"))
  expect_identical(out$method, c("percentile", "t"))
  expect_equal(nrow(out), 2L)
})

test_that("comparison is reproducible", {
  x <- c(3, 5, 8, 12, 17, 21, 25, 30)
  set.seed(1998)
  first <- compare_mean_ci(x, B = 2000)
  set.seed(1998)
  second <- compare_mean_ci(x, B = 2000)
  expect_identical(first, second)
})

test_that("invalid methods are rejected", {
  expect_error(compare_mean_ci(1:10,
    methods = "unknown"))
  expect_error(compare_mean_ci(1:10,
    methods = c("basic", "basic")))
  expect_error(compare_mean_ci(1:10,
    methods = character(0)))
})

test_that("constant data behavior is appropriate", {
  x <- rep(10, 20)
  out <- compare_mean_ci(x, B = 200,
    methods = c("percentile", "basic", "normal", "bca"))
  expect_true(all(out$lower == 10))
  expect_true(all(out$upper == 10))
  expect_error(compare_mean_ci(x, B = 200,
    methods = "t"), "constant sample")
})

test_that("t-only comparison does not consume RNG", {
  set.seed(1998)
  before <- .Random.seed
  out <- compare_mean_ci(1:20, methods = "t")
  expect_identical(before, .Random.seed)
  expect_equal(nrow(out), 1L)
})
