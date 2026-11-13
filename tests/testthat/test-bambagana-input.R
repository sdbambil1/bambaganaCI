
# bambaganaCI simple input interface v1

testthat::test_that("simple mean inputs preserve results", {

  x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)
  y <- c(71, 79, 75, 82, 77, 85, 80, 74, 78, 81)

  testthat::expect_equal(
    bambagana("mean", x),
    mean_t_ci(x)
  )

  actual <- bambagana("compare means", x, y)
  expected <- mean_difference_ci(x, y)
  
  # Group names are display metadata.
  attr(actual, "console_groups") <- NULL
  attr(expected, "console_groups") <- NULL
  
  testthat::expect_equal(actual, expected)

  testthat::expect_equal(
    bambagana("test mean", x, mu = 70)$p.value,
    mean_test(x, mu = 70)$p.value
  )

  testthat::expect_equal(
    bambagana("test two means", x, y)$p.value,
    two_mean_test(x, y)$p.value
  )
})

testthat::test_that("count pairs are interpreted correctly", {

  result <- bambagana(
    "compare proportions",
    c(65, 100),
    c(45, 100)
  )

  testthat::expect_equal(
    as.numeric(result$estimate[1L]),
    0.20,
    tolerance = 1e-12
  )

  testthat::expect_error(
    bambagana("proportion", c(120, 100)),
    "Successes cannot exceed trials"
  )
})

testthat::test_that("methods and invalid tasks behave correctly", {

  choices <- suppressMessages(
    capture.output(bambagana("methods"))
  )

  testthat::expect_true(length(choices) > 0L)

  testthat::expect_error(
    bambagana("unknown", 1:10),
    "Unknown analysis"
  )
})

