test_that("normal critical values remain finite near 100 percent confidence", {
  set.seed(1998)
  out <- bootstrap_mean_ci(1:20, method = "normal", conf = 1 - .Machine$double.eps/2)
  expect_true(all(is.finite(c(out$lower, out$upper))))
  expect_lt(out$lower, out$upper)
})

test_that("large offsets preserve the relevant sample variation", {
  x <- (1:20) * 4
  shift <- 1e16
  for (method in c("percentile", "basic", "normal", "bca")) {
    set.seed(1998)
    base <- bootstrap_mean_ci(x, method = method)
    set.seed(1998)
    shifted <- bootstrap_mean_ci(x + shift, method = method)
    # Endpoints can only be represented to the local floating-point spacing.
    expect_lte(max(abs(c(shifted$lower, shifted$upper) - shift -
                       c(base$lower, base$upper))), 2)
    expect_equal(shifted$bootstrap_se, base$bootstrap_se, tolerance = 1e-12)
    expect_true(shifted$diagnostics$rescaled)
  }
  out <- mean_t_ci(x + shift)
  expect_equal(out$standard_error, stats::sd(x)/sqrt(length(x)), tolerance = 1e-12)
})

test_that("no bootstrap variation is reported as an explicit failure", {
  # Search a deterministic stream for two equal means from nonconstant data.
  # Only the audit controls the seed; package calls do not reset it.
  set.seed(1998)
  caught <- FALSE
  for (i in seq_len(100)) {
    out <- tryCatch(suppressWarnings(bootstrap_mean_ci(c(0, 1), B = 2)),
                    error = identity)
    if (inherits(out, "error")) {
      expect_match(conditionMessage(out), "no representable variation")
      caught <- TRUE
      break
    }
  }
  expect_true(caught)
})

test_that("constant and precision diagnostics are retained", {
  for (method in c("percentile", "basic", "normal", "bca")) {
    set.seed(1998)
    out <- bootstrap_mean_ci(rep(1e200, 5), method = method)
    expect_true(out$diagnostics$constant_data)
    expect_true(out$diagnostics$degenerate_interval)
    expect_equal(out$bootstrap_se, 0)
  }
  set.seed(1998)
  expect_error(bootstrap_mean_ci(c(0, 5e-324), method = "normal"), "precision")
})

test_that("standard extractors preserve estimates and stored levels", {
  set.seed(1998)
  bootstrap <- bootstrap_mean_ci(1:10, conf = .9)
  standard <- mean_t_ci(1:10, conf = .9)
  for (out in list(bootstrap, standard)) {
    expect_equal(unname(stats::coef(out)), out$estimate)
    expect_equal(as.numeric(stats::confint(out)), c(out$lower, out$upper))
    expect_equal(stats::confint(out, parm = 1), stats::confint(out))
    expect_error(stats::confint(out, level = .95), "stored confidence")
    expect_error(stats::confint(out, parm = "slope"), "parm")
    expect_equal(as.data.frame(out)$n, 10)
    expect_equal(as.data.frame(out)$method, out$method)
  }
  expect_output(print(standard), "standard t")
  invisible(capture.output(visible <- withVisible(print(standard))$visible))
  expect_identical(visible, FALSE)
})


test_that("complex inputs are rejected before arithmetic", {
  expect_error(bootstrap_mean_ci(c(1+1i, 2)), "numeric vector")
  expect_error(bootstrap_mean_ci(1:10, B = 2000+1i), "whole number")
  expect_error(bootstrap_mean_ci(1:10, conf = .95+1i), "single number")
  expect_error(mean_t_ci(c(1+1i, 2)), "finite numeric")
  expect_error(mean_t_ci(1:10, conf = .95+1i), "strictly between")
})

test_that("BCa matches an independent jackknife reference under stress", {
  set.seed(1998)
  samples <- list(stats::rt(30, df = 3)/sqrt(3),
                  stats::rnorm(30, sd = ifelse(stats::rbinom(30, 1, .05), 5, 1))/sqrt(2.2))
  for (x in samples) {
    set.seed(1998)
    out <- bootstrap_mean_ci(x, method = "bca")
    set.seed(1998)
    reference <- boot::boot(x, function(d, i) mean(d[i]), R = 2000,
                            simple = TRUE, parallel = "no")
    influence <- boot::empinf(reference, type = "jack")
    interval <- boot::boot.ci(reference, type = "bca", index = 1L, L = influence)
    expect_equal(out$bootstrap_means, as.numeric(reference$t), tolerance = 1e-12)
    expect_equal(c(out$lower, out$upper), as.numeric(interval$bca[1, 4:5]),
                 tolerance = 1e-10)
  }
})
