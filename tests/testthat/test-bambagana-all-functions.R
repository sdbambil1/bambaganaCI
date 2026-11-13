

testthat::test_that(
  "all statistical functions have friendly commands",
  {

    exports <- getNamespaceExports("bambaganaCI")

    statistical <- setdiff(
      exports,
      c(
        "bambagana",
        "bambagana_show",
        "bambagana_catalog"
      )
    )

    inventory <- bambagana(
      "all functions",
      display = FALSE
    )

    testthat::expect_equal(
      nrow(inventory),
      40L
    )

    testthat::expect_setequal(
      inventory$R_function,
      statistical
    )

    testthat::expect_false(
      anyDuplicated(inventory$Command) > 0L
    )

    testthat::expect_true(
      all(nzchar(inventory$Arguments))
    )
  }
)

testthat::test_that(
  "confidence interval aliases match the original result",
  {

    scores <- c(
      65, 72, 68, 75, 70,
      78, 74, 69, 73, 76
    )

    original <- bambagana(
      "mean",
      scores,
      display = FALSE
    )

    for (command in c(
      "confidence interval",
      "confidence intervals",
      "ci",
      "mean confidence interval",
      "confidence interval for mean"
    )) {

      actual <- bambagana(
        command,
        scores,
        display = FALSE
      )

      testthat::expect_equal(
        actual,
        original
      )
    }
  }
)

testthat::test_that(
  "descriptive statistics aliases are equivalent",
  {

    x <- c(65, 72, 68, 75, 70)

    actual <- bambagana(
      "descriptive statistics",
      x,
      display = FALSE
    )

    expected <- describe_numeric(x)

    testthat::expect_equal(
      actual,
      expected
    )
  }
)

testthat::test_that(
  "standard error aliases retain original calculations",
  {

    x <- c(65, 72, 68, 75, 70)

    actual <- bambagana(
      "standard error",
      x,
      display = FALSE
    )

    expected <- mean_se(x)

    testthat::expect_equal(
      actual,
      expected
    )
  }
)

testthat::test_that(
  "previous commands and result objects remain intact",
  {

    x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)
    y <- x + 5

    reference <- bambagana(
      "compare means",
      x,
      y,
      display = FALSE
    )

    result <- bambagana(
      "compare averages",
      x,
      y,
      display = FALSE
    )

    testthat::expect_equal(result, reference)

    testthat::expect_equal(
      length(getNamespaceExports("bambaganaCI")),
      43L
    )
  }
)

testthat::test_that(
  "all functions can be discovered without printed output",
  {

    output <- capture.output(
      inventory <- bambagana(
        "all functions",
        display = FALSE
      )
    )

    testthat::expect_length(output, 0L)
    testthat::expect_equal(nrow(inventory), 40L)
  }
)



# FRIENDLY ALIAS NORMALIZATION REGRESSION

testthat::test_that(
  "friendly aliases resolve technical function names",
  {
    x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)

    result <- bambagana(
      "standard error", x, display = FALSE
    )

    testthat::expect_equal(result, mean_se(x))

    result2 <- bambagana(
      "standard error of mean", x, display = FALSE
    )

    testthat::expect_equal(result2, mean_se(x))

    result3 <- bambagana(
      "mean se", x, display = FALSE
    )

    testthat::expect_equal(result3, mean_se(x))

    reference <- bambagana(
      "mean", x, display = FALSE
    )

    testthat::expect_equal(
      bambagana("confidence interval",
                x, display = FALSE),
      reference
    )

    testthat::expect_equal(
      bambagana("ci", x, display = FALSE),
      reference
    )
  }
)
