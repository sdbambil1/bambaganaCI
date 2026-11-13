
testthat::test_that(
  "unified display preserves original interval objects",
  {

    x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)

    result <- mean_t_ci(x)

    original <- serialize(result, NULL)

    text <- capture.output(
      returned <- bambagana_show(result)
    )

    testthat::expect_identical(
      serialize(result, NULL), original
    )

    testthat::expect_identical(returned, result)

    testthat::expect_true(
      any(grepl("Confidence interval:", text, fixed = TRUE))
    )
  }
)

testthat::test_that(
  "unified display handles ANOVA before lm",
  {

    x <- c(
      65, 72, 68, 75,
      70, 78, 74, 69,
      71, 79, 75, 82
    )

    group <- factor(rep(c("A", "B", "C"), each = 4))

    model <- anova_oneway(x, group)

    text <- capture.output(
      bambagana_show(model)
    )

    testthat::expect_true(
      any(grepl("Analysis of variance", text, fixed = TRUE))
    )
  }
)

testthat::test_that(
  "unified display presents regression results",
  {

    d <- data.frame(
      x = 1:8,
      y = c(3, 5, 6, 9, 10, 12, 15, 17)
    )

    model <- ols_fit(y ~ x, data = d)

    text <- capture.output(
      bambagana_show(model)
    )

    testthat::expect_true(
      any(grepl("R-squared", text, fixed = TRUE))
    )
  }
)

testthat::test_that(
  "unified display shows hypothesis-test information",
  {

    x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)

    result <- mean_test(x, mu = 70)

    text <- capture.output(
      bambagana_show(result)
    )

    testthat::expect_true(
      any(grepl("Null value:", text, fixed = TRUE))
    )

    testthat::expect_true(
      any(grepl("P-value:", text, fixed = TRUE))
    )
  }
)

testthat::test_that(
  "technical display preserves standard printing",
  {

    x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)

    result <- mean_test(x, mu = 70)

    testthat::expect_output(
      bambagana_show(result, details = TRUE),
      "One Sample t-test"
    )
  }
)

testthat::test_that(
  "unified display retains the method catalog",
  {

    catalog <- bambagana_catalog()

    text <- capture.output(
      bambagana_show(catalog)
    )

    testthat::expect_equal(nrow(catalog), 40L)

    testthat::expect_true(
      any(grepl("Available methods:", text, fixed = TRUE))
    )
  }
)

testthat::test_that(
  "unified display validates its arguments",
  {

    testthat::expect_error(
      bambagana_show(1, digits = 0),
      "digits"
    )

    testthat::expect_error(
      bambagana_show(1, details = NA),
      "details"
    )
  }
)

