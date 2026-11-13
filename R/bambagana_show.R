
#' Display a bambaganaCI Statistical Result
#'
#' Provides a consistent console display without modifying
#' the underlying statistical result.
#'
#' @param result A statistical result object.
#' @param details Show the original technical output.
#' @param digits Number of significant digits.
#' @param title Optional display heading.
#' @return Invisibly returns the original result.
#' @export
bambagana_show <- function(result, details = FALSE,
                           digits = 4L, title = NULL) {

  if (!is.logical(details) || length(details) != 1L ||
      is.na(details)) {
    stop("details must be TRUE or FALSE.")
  }

  if (!is.numeric(digits) || length(digits) != 1L ||
      !is.finite(digits) || digits != floor(digits) ||
      digits < 1L || digits > 15L) {
    stop("digits must be a whole number between 1 and 15.")
  }

  if (!is.null(title) &&
      (!is.character(title) || length(title) != 1L ||
       is.na(title) || !nzchar(title))) {
    stop("title must be NULL or a nonempty string.")
  }

  number <- function(z) {
    if (length(z) != 1L || is.na(z)) return("NA")
    if (is.infinite(z)) {
      return(if (z > 0) "Inf" else "-Inf")
    }
    format(signif(as.numeric(z), digits),
           trim = TRUE, scientific = FALSE)
  }

  p_value <- function(p) {
    if (length(p) != 1L || is.na(p)) return("NA")
    if (p < 0.0001) return("< 0.0001")
    sprintf("%.4f", p)
  }

  interval <- function(lower, upper) {
    paste0("[", number(lower), ", ", number(upper), "]")
  }

  field <- function(label, value) {
    cat(sprintf("%-24s %s\n", paste0(label, ":"), value))
  }

  heading <- function(label) {
    label <- toupper(label)
    cat("\n", label, "\n", sep = "")
    cat(strrep("-", nchar(label)), "\n", sep = "")
  }

  if (is.null(title)) {
    title <- if (inherits(result, "aov")) {
      "One-way ANOVA"
    } else if (inherits(result, "lm")) {
      "Linear regression"
    } else if (inherits(result, "htest")) {
      result$method
    } else if (inherits(result, "bootmean_ci")) {
      "Bootstrap confidence interval"
    } else if (inherits(result, "mean_ci") ||
               inherits(result, "bambagana_interval")) {
      "Confidence interval"
    } else if (is.data.frame(result) &&
               "function_name" %in% names(result)) {
      "Statistical method catalog"
    } else {
      "Statistical result"
    }
  }

  heading(title)

  # Technical mode preserves standard object printing.
  if (details) {
    if (inherits(result, "aov") ||
        inherits(result, "lm")) {
      print(summary(result))
    } else if (
      inherits(result, "bambagana_comparison_test") ||
      inherits(result, "bambagana_comparison_ci")
    ) {
      print(result, details = TRUE)
    } else {
      print(result)
    }
    return(invisible(result))
  }

  # ANOVA must be checked before lm: aov inherits lm.
  if (inherits(result, "aov")) {

    tab <- summary(result)[[1L]]

    cat("\nAnalysis of variance:\n\n")

    values <- data.frame(
      Source = rownames(tab),
      Df = as.character(tab[["Df"]]),
      Sum_Sq = vapply(
        tab[["Sum Sq"]], number, character(1)
      ),
      Mean_Sq = vapply(
        tab[["Mean Sq"]], number, character(1)
      ),
      F = vapply(
        tab[["F value"]], function(z) {
          if (is.na(z)) "" else number(z)
        }, character(1)
      ),
      P_value = vapply(
        tab[["Pr(>F)"]], function(z) {
          if (is.na(z)) "" else p_value(z)
        }, character(1)
      ),
      check.names = FALSE
    )

    print(values, row.names = FALSE, right = FALSE)

    p <- tab[["Pr(>F)"]][1L]

    cat("\n")
    field("F-statistic", number(tab[["F value"]][1L]))
    field("P-value", p_value(p))
    field("Method", "One-way ANOVA")

    if (is.finite(p)) {
      cat("\nConclusion: ")
      if (p < 0.05) {
        cat("Evidence that the population means",
            "are not all equal at the 5% level.\n")
      } else {
        cat("Insufficient evidence that the population",
            "means differ at the 5% level.\n")
      }
    }

    return(invisible(result))
  }

  # Regression
  if (inherits(result, "lm")) {

    s <- summary(result)
    coefs <- s$coefficients

    cat("\nCoefficient estimates:\n\n")

    values <- data.frame(
      Term = rownames(coefs),
      Estimate = vapply(
        coefs[, 1L], number, character(1)
      ),
      SE = vapply(
        coefs[, 2L], number, character(1)
      ),
      t = vapply(
        coefs[, 3L], number, character(1)
      ),
      P_value = vapply(
        coefs[, 4L], p_value, character(1)
      ),
      check.names = FALSE
    )

    print(values, row.names = FALSE, right = FALSE)

    cat("\nModel fit:\n")
    field("Observations", as.character(stats::nobs(result)))
    field("R-squared", number(s$r.squared))
    field("Adjusted R-squared", number(s$adj.r.squared))
    field("Residual SE", number(s$sigma))
    field("Residual df", as.character(result$df.residual))

    return(invisible(result))
  }

  # Hypothesis tests
  if (inherits(result, "htest")) {

    est <- result$estimate

    if (length(est)) {
      for (i in seq_along(est)) {
        label <- names(est)[i]

        if (length(label) == 0L || is.na(label) ||
            !nzchar(label)) {
          label <- paste("Estimate", i)
        }

        field(label, number(est[i]))
      }
    }

    if (length(est) == 2L &&
        grepl("Two Sample", result$method,
              ignore.case = TRUE)) {
      field("Difference (first - second)",
            number(est[1L] - est[2L]))
    }

    if (length(result$null.value)) {
      field("Null value",
            paste(vapply(result$null.value,
                         number, character(1)),
                  collapse = ", "))
    }

    if (length(result$alternative)) {
      field("Alternative", result$alternative)
    }

    if (length(result$conf.int) == 2L) {
      level <- attr(result$conf.int, "conf.level")

      if (!is.null(level)) {
        field("Confidence level",
              paste0(number(level * 100), "%"))
      }

      field("Confidence interval",
            interval(result$conf.int[1L],
                     result$conf.int[2L]))
    }

    if (length(result$statistic)) {
      field("Test statistic",
            number(result$statistic[1L]))
    }

    if (length(result$parameter)) {
      field("Degrees of freedom",
            paste(vapply(result$parameter,
                         number, character(1)),
                  collapse = ", "))
    }

    field("P-value", p_value(result$p.value))
    field("Method", result$method)

    if (is.finite(result$p.value)) {
      cat("\nConclusion: ")

      if (result$p.value < 0.05) {
        cat("Evidence against the null hypothesis",
            "at the 5% level.\n")
      } else {
        cat("Insufficient evidence against the null",
            "hypothesis at the 5% level.\n")
      }
    }

    return(invisible(result))
  }

  # Confidence intervals
  if (inherits(result, "mean_ci") ||
      inherits(result, "bambagana_interval")) {

    if (!is.null(result$estimate)) {
      field("Estimate", number(result$estimate[1L]))
    }

    if (!is.null(result$lower) &&
        !is.null(result$upper)) {
      field("Confidence interval",
            interval(result$lower[1L],
                     result$upper[1L]))
    }

    if (!is.null(result$conf)) {
      field("Confidence level",
            paste0(number(result$conf[1L] * 100), "%"))
    }

    if (!is.null(result$standard_error)) {
      field("Standard error",
            number(result$standard_error))
    }

    if (!is.null(result$bootstrap_se)) {
      field("Bootstrap SE",
            number(result$bootstrap_se))
    }

    if (!is.null(result$n)) {
      field("Sample size", as.character(result$n))
    }

    if (!is.null(result$B)) {
      field("Bootstrap resamples", as.character(result$B))
    }

    if (!is.null(result$method)) {
      field("Method", as.character(result$method[1L]))
    }

    return(invisible(result))
  }

  # Function catalog
  if (is.data.frame(result) &&
      all(c("function_name", "category", "description") %in%
          names(result))) {

    field("Available methods", as.character(nrow(result)))

    categories <- unique(result$category)

    for (category in categories) {
      methods <- result$function_name[
        result$category == category
      ]

      cat("\n", category, " (", length(methods),
          ")\n", sep = "")

      cat(
        paste0("  ", methods, collapse = "\n"),
        "\n"
      )
    }

    return(invisible(result))
  }

  # Ordinary numeric estimates
  if (is.numeric(result) && length(result) == 1L) {
    field("Estimate", number(result))
    return(invisible(result))
  }

  # Other data frames and objects
  print(result)

  invisible(result)
}

