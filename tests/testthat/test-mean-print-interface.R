
testthat::test_that(
  "mean interval printing supports default and detail modes",
  {

    x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)

    result <- mean_t_ci(x)
    original <- serialize(result, NULL)

    default <- capture.output(print(result))

    detailed <- capture.output(
      print(result, details = TRUE)
    )

    testthat::expect_true(
      any(grepl("Confidence interval:", default,
                fixed = TRUE))
    )

    testthat::expect_true(
      any(grepl("Degrees of freedom:", detailed,
                fixed = TRUE))
    )

    testthat::expect_true(
      length(detailed) > length(default)
    )

    testthat::expect_identical(
      serialize(result, NULL),
      original
    )
  }
)

testthat::test_that(
  "bootstrap printing supports default and detail modes",
  {

    x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)

    set.seed(1998)

    result <- bootstrap_mean_ci(
      x,
      B = 399,
      method = "bca"
    )

    original <- serialize(result, NULL)

    default <- capture.output(print(result))

    detailed <- capture.output(
      print(result, details = TRUE)
    )

    testthat::expect_true(
      any(grepl("Bootstrap resamples:", default,
                fixed = TRUE))
    )

    testthat::expect_true(
      any(grepl("Diagnostics:", detailed,
                fixed = TRUE))
    )

    testthat::expect_identical(
      serialize(result, NULL),
      original
    )
  }
)

