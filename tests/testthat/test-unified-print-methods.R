
testthat::test_that(
  "custom interval printing supports unified display",
  {

    x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)
    y <- c(71, 79, 75, 82, 77, 85, 80, 74, 78, 81)

    results <- list(
      paired = paired_mean_ci(x, y),
      difference = mean_difference_ci(x, y),
      comparison = two_mean_test(x, y)
    )

    for (name in names(results)) {

      result <- results[[name]]
      original <- serialize(result, NULL)

      default <- capture.output(
        print(result)
      )

      technical <- capture.output(
        print(result, details = TRUE)
      )

      testthat::expect_true(
        length(default) > 0L,
        info = name
      )

      testthat::expect_true(
        length(technical) > 0L,
        info = name
      )

      testthat::expect_true(
        any(grepl(
          "interval:",
          default,
          ignore.case = TRUE
        )),
        info = name
      )

      testthat::expect_identical(
        serialize(result, NULL),
        original,
        info = name
      )
    }
  }
)

testthat::test_that(
  "comparison display shows inferential information",
  {

    x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)
    y <- c(71, 79, 75, 82, 77, 85, 80, 74, 78, 81)

    result <- two_mean_test(x, y)

    text <- capture.output(print(result))

    testthat::expect_true(
      any(grepl("P-value:", text, fixed = TRUE))
    )

    testthat::expect_true(
      any(grepl("Difference", text, fixed = TRUE))
    )

    testthat::expect_true(
      any(grepl("Null value:", text, fixed = TRUE))
    )
  }
)

testthat::test_that(
  "print details validation rejects invalid values",
  {

    x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)
    y <- c(71, 79, 75, 82, 77, 85, 80, 74, 78, 81)

    result <- mean_difference_ci(x, y)

    testthat::expect_error(
      print(result, details = NA),
      "details"
    )
  }
)

