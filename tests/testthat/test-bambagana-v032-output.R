
# bambaganaCI v0.3.2 output regression tests

testthat::test_that(
  "descriptive output is readable and numerically unchanged",
  {

    x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)

    result <- bambagana(
      "describe", x,
      display = FALSE
    )

    testthat::expect_equal(
      result,
      describe_numeric(x)
    )

    output <- capture.output(
      bambagana("describe", x)
    )

    for (label in c(
      "DESCRIPTIVE STATISTICS",
      "Observations:",
      "Mean:",
      "Median:",
      "Standard deviation:",
      "Variance:",
      "Minimum:",
      "Maximum:",
      "First quartile:",
      "Third quartile:"
    )) {
      testthat::expect_true(
        any(grepl(label, output, fixed = TRUE)),
        info = label
      )
    }
  }
)

testthat::test_that(
  "proportion formatting retains all essential information",
  {

    p <- bambagana(
      "proportion",
      c(65, 100),
      display = FALSE
    )

    original <- proportion_ci(65, 100)

    testthat::expect_equal(p, original)

    output <- capture.output(
      bambagana("proportion", c(65, 100))
    )

    testthat::expect_true(
      any(grepl("65 / 100", output, fixed = TRUE))
    )

    testthat::expect_true(
      any(grepl("65%", output, fixed = TRUE))
    )

    testthat::expect_true(
      any(grepl("confidence interval", output, fixed = TRUE))
    )
  }
)

testthat::test_that(
  "proportion differences retain percentage-point units",
  {

    result <- bambagana(
      "compare proportions",
      c(65, 100),
      c(45, 100),
      display = FALSE
    )

    testthat::expect_equal(
      as.numeric(result$estimate),
      0.20,
      tolerance = 1e-12
    )

    output <- capture.output(
      bambagana(
        "compare proportions",
        c(65, 100),
        c(45, 100)
      )
    )

    testthat::expect_true(
      any(grepl("20 percentage points", output, fixed = TRUE))
    )

    testthat::expect_true(
      any(grepl("65%", output, fixed = TRUE))
    )

    testthat::expect_true(
      any(grepl("45%", output, fixed = TRUE))
    )
  }
)

testthat::test_that(
  "two-sample interpretation preserves statistical results",
  {

    x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)
    y <- c(71, 79, 75, 82, 77, 85, 80, 74, 78, 81)

    result <- bambagana(
      "test two means",
      x, y,
      display = FALSE
    )

    reference <- two_mean_test(x, y)

    # Group labels are display metadata, not statistics.
    attr(result, "console_groups") <- NULL
    attr(reference, "console_groups") <- NULL

    testthat::expect_equal(result, reference)

    output <- capture.output(
      bambagana("test two means", x, y)
    )

    testthat::expect_true(
      any(grepl("Group 2 has a sample mean", output,
                fixed = TRUE))
    )

    testthat::expect_true(
      any(grepl("6.2 units higher", output,
                fixed = TRUE))
    )

    testthat::expect_true(
      any(grepl("population means differ", output,
                fixed = TRUE))
    )
  }
)

testthat::test_that(
  "no added interpretation when display is disabled",
  {

    x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)
    y <- c(71, 79, 75, 82, 77, 85, 80, 74, 78, 81)

    output <- capture.output(
      result <- bambagana(
        "test two means",
        x, y,
        display = FALSE
      )
    )

    testthat::expect_length(output, 0L)
    testthat::expect_s3_class(result, "htest")
  }
)

testthat::test_that(
  "the public API and catalog remain unchanged",
  {

    testthat::expect_length(
      getNamespaceExports("bambaganaCI"),
      43L
    )

    testthat::expect_equal(
      nrow(bambagana_catalog()),
      40L
    )
  }
)

