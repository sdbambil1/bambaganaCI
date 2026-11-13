
# bambaganaCI Phase 3 usability regression tests

testthat::test_that(
  "simple input keeps numerical results unchanged",
  {

    x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)
    y <- c(71, 79, 75, 82, 77, 85, 80, 74, 78, 81)

    a <- bambagana("mean", x, display = FALSE)
    b <- mean_t_ci(x)

    testthat::expect_equal(a, b)

    a <- bambagana(
      "compare means", x, y, display = FALSE
    )

    b <- mean_difference_ci(x, y)

    attr(a, "console_groups") <- NULL
    attr(b, "console_groups") <- NULL

    testthat::expect_equal(a, b)

    a <- bambagana(
      "compare proportions",
      c(65, 100),
      c(45, 100),
      display = FALSE
    )

    b <- proportion_difference_ci(
      successes1 = 65,
      trials1 = 100,
      successes2 = 45,
      trials2 = 100
    )

    testthat::expect_equal(a, b)
  }
)

testthat::test_that(
  "automatic output is shown but not added to results",
  {

    x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)

    text <- capture.output(
      result <- bambagana("mean", x)
    )

    testthat::expect_true(
      any(grepl(
        "Confidence interval:",
        text,
        fixed = TRUE
      ))
    )

    testthat::expect_equal(
      result,
      mean_t_ci(x)
    )

    quiet <- capture.output(
      result2 <- bambagana(
        "mean",
        x,
        display = FALSE
      )
    )

    testthat::expect_length(quiet, 0L)
    testthat::expect_equal(result2, result)
  }
)

testthat::test_that(
  "hypothesis tests have improved display",
  {

    x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)

    text <- capture.output(
      result <- bambagana(
        "test mean",
        x,
        mu = 70
      )
    )

    testthat::expect_true(
      any(grepl("P-value:", text, fixed = TRUE))
    )

    testthat::expect_true(
      any(grepl("Conclusion:", text, fixed = TRUE))
    )

    testthat::expect_s3_class(result, "htest")
    testthat::expect_equal(
      result$p.value,
      mean_test(x, mu = 70)$p.value
    )
  }
)

testthat::test_that(
  "invalid inputs explain how to correct them",
  {

    x <- c(65, 72, 68, 75, 70)

    testthat::expect_error(
      bambagana("unknown", x),
      "bambagana\\('methods'\\)"
    )

    testthat::expect_error(
      bambagana("compare means", x),
      "Example:"
    )

    testthat::expect_error(
      bambagana("proportion", c(120, 100)),
      "Successes cannot exceed trials"
    )

    testthat::expect_error(
      bambagana("mean", x, conf = 2),
      "between 0 and 1"
    )

    testthat::expect_error(
      bambagana("paired", x, 1:3),
      "same length"
    )

    testthat::expect_error(
      bambagana("regression", x),
      "formula"
    )

    testthat::expect_error(
      bambagana("mean", x, display = NA),
      "TRUE or FALSE"
    )
  }
)

testthat::test_that(
  "the simple interface preserves existing API",
  {

    testthat::expect_true(
      "bambagana" %in%
        getNamespaceExports("bambaganaCI")
    )

    testthat::expect_length(
      getNamespaceExports("bambaganaCI"),
      43L
    )

    testthat::expect_equal(
      length(bambagana_catalog()$function_name),
      40L
    )
  }
)

