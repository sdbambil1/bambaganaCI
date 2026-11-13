
testthat::test_that("catalog contains all original functions", {

  result <- bambagana_catalog()

  testthat::expect_s3_class(
    result,
    "data.frame"
  )

  testthat::expect_equal(
    nrow(result),
    40L
  )

  testthat::expect_false(
    anyDuplicated(result$function_name) > 0L
  )

  testthat::expect_false(
    anyNA(result)
  )

  exports <- setdiff(
    getNamespaceExports("bambaganaCI"),
    "bambagana_catalog"
  )

  exports <- setdiff(exports, c("bambagana_show", "bambagana"))
  testthat::expect_setequal(
    result$function_name,
    exports
  )
})

testthat::test_that("catalog searches correctly", {

  testthat::expect_true(
    "mean_t_ci" %in%
      bambagana_catalog("confidence")$function_name
  )

  testthat::expect_true(
    "ols_fit" %in%
      bambagana_catalog("regression")$function_name
  )

  testthat::expect_true(
    "proportion_ci" %in%
      bambagana_catalog("proportion")$function_name
  )

  testthat::expect_equal(
    nrow(bambagana_catalog("unknown_method_999")),
    0L
  )

  testthat::expect_equal(
    bambagana_catalog("REGRESSION"),
    bambagana_catalog("regression")
  )
})

testthat::test_that("invalid searches produce errors", {

  testthat::expect_error(
    bambagana_catalog("")
  )

  testthat::expect_error(
    bambagana_catalog(NA_character_)
  )

  testthat::expect_error(
    bambagana_catalog(123)
  )

  testthat::expect_error(
    bambagana_catalog(c("mean", "test"))
  )
})

