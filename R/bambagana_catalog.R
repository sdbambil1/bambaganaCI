
#' Explore Available Statistical Methods
#'
#' Search the statistical methods provided by bambaganaCI.
#' This is a discovery tool, not an automatic method selector.
#'
#' @param search NULL or a nonempty character string.
#'
#' @return A data frame containing function names,
#' categories, and descriptions.
#'
#' @export
bambagana_catalog <- function(search = NULL) {

  groups <- list(
    "Descriptive statistics" = c(
      "describe_numeric",
      "mean_se",
      "mc_se",
      "covariance_estimate",
      "correlation_estimate"
    ),

    "Confidence intervals" = c(
      "mean_t_ci",
      "mean_z_ci",
      "mean_difference_ci",
      "paired_mean_ci",
      "median_ci",
      "variance_ci",
      "sd_ci",
      "variance_ratio_ci",
      "proportion_ci",
      "proportion_difference_ci",
      "correlation_ci"
    ),

    "Hypothesis tests" = c(
      "mean_test",
      "two_mean_test",
      "paired_mean_test",
      "variance_test",
      "variance_ratio_test",
      "proportion_test",
      "two_proportion_test",
      "correlation_test",
      "sign_test",
      "wilcoxon_one_test",
      "wilcoxon_two_test",
      "wilcoxon_paired_test",
      "chisq_independence_test"
    ),

    "Regression and ANOVA" = c(
      "ols_fit",
      "ols_coef_ci",
      "ols_predict_ci",
      "anova_oneway",
      "anova_tukey"
    ),

    "Bootstrap methods" = c(
      "bootstrap_mean_ci"
    ),

    "Planning and utilities" = c(
      "sample_size_mean",
      "mean_power",
      "ci_width",
      "ci_contains",
      "compare_mean_ci"
    )
  )

  descriptions <- c(
    describe_numeric = "Summarize numerical observations",
    mean_se = "Calculate the standard error of a mean",
    mc_se = "Calculate Monte Carlo standard error",
    covariance_estimate = "Estimate covariance",
    correlation_estimate = "Estimate correlation",

    mean_t_ci = "Confidence interval for a population mean",
    mean_z_ci = "Known-sigma confidence interval for a mean",
    mean_difference_ci = "Confidence interval for two independent means",
    paired_mean_ci = "Confidence interval for paired differences",
    median_ci = "Confidence interval for a population median",
    variance_ci = "Confidence interval for a population variance",
    sd_ci = "Confidence interval for a standard deviation",
    variance_ratio_ci = "Confidence interval for a variance ratio",
    proportion_ci = "Confidence interval for a proportion",
    proportion_difference_ci = "Confidence interval for a difference in proportions",
    correlation_ci = "Confidence interval for Pearson correlation",

    mean_test = "One-sample mean hypothesis test",
    two_mean_test = "Two-sample mean hypothesis test",
    paired_mean_test = "Paired mean hypothesis test",
    variance_test = "Hypothesis test for a variance",
    variance_ratio_test = "Hypothesis test for a variance ratio",
    proportion_test = "One-sample proportion test",
    two_proportion_test = "Two-sample proportion test",
    correlation_test = "Test of population correlation",
    sign_test = "Nonparametric sign test",
    wilcoxon_one_test = "One-sample Wilcoxon test",
    wilcoxon_two_test = "Two-sample Wilcoxon test",
    wilcoxon_paired_test = "Paired Wilcoxon test",
    chisq_independence_test = "Chi-square test of independence",

    ols_fit = "Fit ordinary least squares regression",
    ols_coef_ci = "Regression coefficient confidence intervals",
    ols_predict_ci = "Regression prediction intervals",
    anova_oneway = "One-way analysis of variance",
    anova_tukey = "Tukey multiple comparisons",

    bootstrap_mean_ci = "Bootstrap confidence intervals for a mean",

    sample_size_mean = "Sample-size planning for a mean",
    mean_power = "Power calculations for a mean",
    ci_width = "Calculate confidence interval width",
    ci_contains = "Check whether an interval contains a value",
    compare_mean_ci = "Compare mean confidence interval methods"
  )

  function_names <- unlist(
    groups,
    use.names = FALSE
  )

  categories <- rep(
    names(groups),
    lengths(groups)
  )

  result <- data.frame(
    function_name = function_names,
    category = categories,
    description = unname(descriptions[function_names]),
    stringsAsFactors = FALSE
  )

  if (!is.null(search)) {

    if (!is.character(search) ||
        length(search) != 1L ||
        is.na(search) ||
        !nzchar(trimws(search))) {
      stop(
        "search must be NULL or one nonempty character string."
      )
    }

    query <- trimws(search)

    query <- tolower(query)

    matched <- grepl(
      query, tolower(result$function_name), fixed = TRUE
    ) | grepl(
      query, tolower(result$category), fixed = TRUE
    ) | grepl(
      query, tolower(result$description), fixed = TRUE
    )

    result <- result[
      matched,
      ,
      drop = FALSE
    ]
  }

  rownames(result) <- NULL

  result
}

