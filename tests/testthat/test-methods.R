test_that("default percentile output remains compatible", {
  set.seed(1998)
  out <- bootstrap_mean_ci(c(10, 20, 30, 40, 50))
  expect_equal(c(out$estimate, out$lower, out$upper), c(30, 18, 42))
  expect_s3_class(out, "bootmean_ci")
  expect_equal(out$n, 5)
  expect_output(print(out), "Method:[[:space:]]+percentile")
})

test_that("constant observations behave consistently across methods", {
  for (method in c("percentile", "basic", "normal", "bca")) {
    set.seed(1998)
    out <- bootstrap_mean_ci(rep(5, 10), method = method)
    expect_equal(c(out$lower, out$upper, out$bootstrap_se), c(5, 5, 0))
  }
})

test_that("BCa agrees with boot's reference on skewed data", {
  set.seed(1998)
  x <- rexp(100)
  set.seed(1998)
  out <- bootstrap_mean_ci(x, B = 2000, method = "bca")
  set.seed(1998)
  ref <- boot::boot(x, function(d, i) mean(d[i]), R = 2000,
                    simple = TRUE, parallel = "no")
  ci <- boot::boot.ci(ref, type = "bca", index = 1L, L = x - mean(x))
  expect_equal(out$bootstrap_means, as.numeric(ref$t), tolerance = 1e-12)
  expect_equal(c(out$lower, out$upper), as.numeric(ci$bca[1, 4:5]),
               tolerance = 1e-10)
})

test_that("basic and normal agree with boot interval formulas", {
  # Use the same saved replicates; compare methods independent of RNG order.
  x <- c(1, 2, 4, 8, 12, 20)
  set.seed(1998)
  basic <- bootstrap_mean_ci(x, B = 2000, method = "basic")
  set.seed(1998)
  ref <- boot::boot(x, function(d, i) mean(d[i]), R = 2000,
                    simple = TRUE, parallel = "no")
  # boot basic uses a different interpolation convention: use type 7 explicitly.
  q <- stats::quantile(as.numeric(ref$t), c(.025, .975), names = FALSE, type = 7)
  expect_equal(c(basic$lower, basic$upper), 2 * mean(x) - rev(q))
  set.seed(1998)
  normal <- bootstrap_mean_ci(x, B = 2000, method = "normal")
  reference_ci <- boot::boot.ci(ref, type = "norm", index = 1L)
  expect_equal(c(normal$lower, normal$upper),
               as.numeric(reference_ci$normal[1, 2:3]), tolerance = 1e-10)
})

test_that("input and tail diagnostics cover additional failures", {
  expect_error(bootstrap_mean_ci(matrix(1:6, 2)), "numeric vector")
  expect_error(bootstrap_mean_ci(c(1, Inf)), "infinite values")
  expect_error(bootstrap_mean_ci(c(1, 2), B = NA_real_), "whole number")
  expect_error(bootstrap_mean_ci(c(1, 2), conf = c(.9, .95)), "single number")
  expect_error(bootstrap_mean_ci(c(1, 2), method = "bca"), "three")
  expect_error(bootstrap_mean_ci(1:5, method = "unknown"))
  set.seed(1998)
  expect_warning(bootstrap_mean_ci(1:5, B = 10), "interval tails")
})
