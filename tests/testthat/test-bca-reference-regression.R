
testthat::test_that(
  "BCa reproduces the boot reference implementation",
  {

    set.seed(1998)

    datasets <- list(
      normal = rnorm(30),
      exponential = rexp(30),
      heavy_t3 = rt(30, df = 3)
    )

    B <- 1999L
    conf <- 0.95

    for (name in names(datasets)) {

      x <- datasets[[name]]

      set.seed(1998)

      actual <- bootstrap_mean_ci(
        x,
        B = B,
        conf = conf,
        method = "bca"
      )

      set.seed(1998)

      reference <- boot::boot(
        x,
        statistic = function(data, indices) {
          mean(data[indices])
        },
        R = B,
        sim = "ordinary",
        simple = TRUE,
        parallel = "no"
      )

      influence <- x - mean(x)
      influence <- influence / max(abs(influence))

      reference_ci <- boot::boot.ci(
        reference,
        conf = conf,
        type = "bca",
        index = 1L,
        L = influence
      )

      expected <- as.numeric(
        reference_ci$bca[1L, 4:5]
      )

      observed <- as.numeric(c(
        actual$lower,
        actual$upper
      ))

      testthat::expect_equal(
        observed,
        expected,
        tolerance = 1e-12,
        info = name
      )

      testthat::expect_equal(
        as.numeric(actual$bootstrap_means),
        as.numeric(reference$t),
        tolerance = 1e-12,
        info = name
      )

      testthat::expect_equal(
        as.numeric(actual$estimate),
        mean(x),
        tolerance = 1e-12,
        info = name
      )

      testthat::expect_true(
        is.finite(actual$lower) &&
          is.finite(actual$upper) &&
          actual$lower <= actual$upper,
        info = name
      )
    }
  }
)

testthat::test_that(
  "BCa is reproducible for a fixed seed",
  {

    x <- c(
      2, 4, 7, 9, 11,
      13, 15, 18, 21, 25
    )

    set.seed(1998)

    first <- bootstrap_mean_ci(
      x,
      B = 1999,
      method = "bca"
    )

    set.seed(1998)

    second <- bootstrap_mean_ci(
      x,
      B = 1999,
      method = "bca"
    )

    testthat::expect_identical(
      first,
      second
    )
  }
)

testthat::test_that(
  "BCa rejects insufficient nonconstant observations",
  {

    testthat::expect_error(
      bootstrap_mean_ci(
        c(1, 2),
        B = 1999,
        method = "bca"
      ),
      "at least three"
    )
  }
)

