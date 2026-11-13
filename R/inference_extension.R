
.bc_vector <- function(x, minimum = 2L) {
  if (!is.numeric(x) || is.complex(x) || !is.null(dim(x)) ||
      length(x) < minimum || any(!is.finite(x)))
    stop("Provide a real numeric vector of finite observations.", call. = FALSE)
  x
}
.bc_scalar <- function(x, label, positive = FALSE, probability = FALSE) {
  if (!is.numeric(x) || is.complex(x) || length(x) != 1L || !is.finite(x) ||
      (positive && x <= 0) || (probability && (x <= 0 || x >= 1)))
    stop(paste("Invalid", label), call. = FALSE)
  x
}
.bc_conf <- function(conf) .bc_scalar(conf, "conf (strictly between 0 and 1)", probability = TRUE)
.bc_flag <- function(x) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) stop("Flag must be TRUE or FALSE.")
  x
}
.bc_counts <- function(successes, trials) {
  .bc_scalar(trials, "trials", positive = TRUE); .bc_scalar(successes, "successes")
  if (trials != floor(trials) || successes != floor(successes) ||
      successes < 0 || successes > trials) stop("Require integer counts: 0 <= successes <= trials.")
}
.bc_pair <- function(x, y) {
  .bc_vector(x); .bc_vector(y)
  if (length(x) != length(y)) stop("Paired samples must have equal lengths.")
}
.bc_ci <- function(estimate, endpoints, conf, method, assumptions,
                   context = NULL, parameter = "estimate", scale = "number") {
  if (anyNA(c(estimate, endpoints)) || length(endpoints) != 2L ||
      endpoints[1] > endpoints[2] || !is.finite(estimate)) stop("Invalid numerical interval result.")
  result <- data.frame(estimate = unname(estimate), lower = unname(endpoints[1]),
    upper = unname(endpoints[2]), conf = conf, method = method,
    assumptions = assumptions, stringsAsFactors = FALSE)
  attr(result, "context") <- context
  attr(result, "parameter") <- parameter
  attr(result, "display_scale") <- scale
  class(result) <- c("bambagana_interval", "data.frame")
  result
}
.bc_lm <- function(model) {
  if (!inherits(model, "lm") || inherits(model, "glm") ||
      model$rank != length(stats::coef(model)) || stats::df.residual(model) <= 0)
    stop("Require a full-rank linear model with positive residual degrees of freedom.")
}

#' Describe numeric
#'
#' @details Finite real observations; sample variance uses denominator n - 1. Missing values are rejected.
#' @param x Real numeric vector of finite observations.
#' @return A numeric summary, interval table, matrix, or planning result as described in Details.
#' @examples
#' describe_numeric(1:10)
#' @export
describe_numeric <- function(x) {
  x <- .bc_vector(x)
  v <- stats::var(x); s <- stats::sd(x)
  if (any(!is.finite(c(mean(x), v, s)))) stop("Summary overflow; rescale your data.")
  data.frame(n = length(x), mean = mean(x), median = stats::median(x),
    sd = s, variance = v, minimum = min(x), maximum = max(x),
    q1 = unname(stats::quantile(x, .25)), q3 = unname(stats::quantile(x, .75)))
}

#' Mean se
#'
#' @details Estimated standard error of an independent sample mean. Finite population variance is assumed. This is not a robust estimator.
#' @param x Real numeric vector of finite observations.
#' @return A numeric summary, interval table, matrix, or planning result as described in Details.
#' @examples
#' mean_se(1:10)
#' @export
mean_se <- function(x) {
  x <- .bc_vector(x); se <- stats::sd(x) / sqrt(length(x))
  if (!is.finite(se) || (se == 0 && !all(x == x[1]))) stop("Standard-error numerical failure; rescale x.")
  se
}

#' Mc se
#'
#' @details Binomial Monte Carlo plug-in standard error. A value of zero at a boundary does not mean the true coverage is known exactly.
#' @param successes Integer number of successes.
#' @param trials Positive integer number of trials.
#' @return A numeric summary, interval table, matrix, or planning result as described in Details.
#' @examples
#' mc_se(950, 1000)
#' @export
mc_se <- function(successes, trials) {
  .bc_counts(successes, trials)
  sqrt((successes / trials) * (1 - successes / trials) / trials)
}

#' Ci width
#'
#' @details Width of a finite ordered interval, in the original measurement units.
#' @param lower Finite lower interval endpoint.
#' @param upper Finite upper interval endpoint.
#' @return A numeric summary, interval table, matrix, or planning result as described in Details.
#' @examples
#' ci_width(18, 42)
#' @export
ci_width <- function(lower, upper) {
  .bc_scalar(lower, "lower"); .bc_scalar(upper, "upper")
  if (lower > upper || !is.finite(upper - lower)) stop("Invalid endpoints or width overflow.")
  upper - lower
}

#' Ci contains
#'
#' @details Inclusive containment of a scalar value in a finite interval.
#' @param lower Finite lower interval endpoint.
#' @param upper Finite upper interval endpoint.
#' @param value Finite scalar to check for inclusion.
#' @return A numeric summary, interval table, matrix, or planning result as described in Details.
#' @examples
#' ci_contains(18, 42, 30)
#' @export
ci_contains <- function(lower, upper, value) {
  .bc_scalar(lower, "lower"); .bc_scalar(upper, "upper"); .bc_scalar(value, "value")
  if (lower > upper) stop("Endpoints are reversed.")
  lower <= value && value <= upper
}

#' Mean z ci
#'
#' @details sigma must be the known population SD, not an estimated sample SD.
#' @param x Real numeric vector of finite observations.
#' @param sigma Known positive population standard deviation.
#' @param conf Confidence level strictly between zero and one.
#' @return A bambagana_interval data frame with estimate, lower, upper, conf,
#' method and assumptions. Printing is concise; as.data.frame returns the
#' numerical table, coef extracts the estimate, and confint extracts stored
#' endpoints without recalculation. median_ci also returns minimum_coverage.
#' @examples
#' mean_z_ci(1:10, sigma = 3)
#' @export
mean_z_ci <- function(x, sigma, conf = 0.95) {
  x <- .bc_vector(x); .bc_scalar(sigma, "sigma", positive = TRUE); .bc_conf(conf)
  margin <- stats::qnorm((1 - conf) / 2, lower.tail = FALSE) * sigma / sqrt(length(x))
  endpoints <- mean(x) + c(-margin, margin)
  if (any(!is.finite(endpoints))) stop("Interval overflow; rescale x and sigma.")
  .bc_ci(mean(x), endpoints, conf, "known-sigma z",
    "Exact for independent normal observations with known positive population SD; otherwise approximate.",
    context = c("Sample size" = length(x)), parameter = "mean")
}

#' Median ci
#'
#' @details Uses binomial order-statistic bounds, not bootstrap resampling. For discrete distributions, continuity-based coverage statements need qualification. Small samples may require unbounded intervals.
#' @param x Real numeric vector of finite observations.
#' @param conf Confidence level strictly between zero and one.
#' @return A bambagana_interval data frame with estimate, lower, upper, conf,
#' method and assumptions. Printing is concise; as.data.frame returns the
#' numerical table, coef extracts the estimate, and confint extracts stored
#' endpoints without recalculation. median_ci also returns minimum_coverage.
#' @examples
#' median_ci(1:20)
#' @export
median_ci <- function(x, conf = 0.95) {
  x <- sort(.bc_vector(x)); .bc_conf(conf); n <- length(x)
  k <- seq_len(floor(n / 2)); achieved <- 1 - 2 * stats::pbinom(k - 1, n, .5)
  allowed <- k[achieved >= conf]
  if (!length(allowed)) {
    warning("This sample is too small for a bounded distribution-free interval at this confidence level.", call. = FALSE)
    endpoints <- c(-Inf, Inf); coverage <- 1
  } else {
    k <- max(allowed); endpoints <- c(x[k], x[n - k + 1]); coverage <- 1 - 2 * stats::pbinom(k - 1, n, .5)
  }
  result <- .bc_ci(stats::median(x), endpoints, conf, "order-statistic median",
    "Independent observations from a continuous distribution with a unique median; nominal coverage is conservative.",
    context = c("Sample size" = n), parameter = "median")
  result$minimum_coverage <- coverage; result
}

#' Variance ci
#'
#' @details Chi-square variance interval relies on normality, and can be sensitive to departures from normality.
#' @param x Real numeric vector of finite observations.
#' @param conf Confidence level strictly between zero and one.
#' @return A bambagana_interval data frame with estimate, lower, upper, conf,
#' method and assumptions. Printing is concise; as.data.frame returns the
#' numerical table, coef extracts the estimate, and confint extracts stored
#' endpoints without recalculation. median_ci also returns minimum_coverage.
#' @examples
#' variance_ci(1:10)
#' @export
variance_ci <- function(x, conf = 0.95) {
  x <- .bc_vector(x); .bc_conf(conf); df <- length(x) - 1; v <- stats::var(x)
  if (!is.finite(v) || v <= 0) stop("Require positive representable sample variance.")
  a <- (1 - conf) / 2
  endpoints <- df * v / c(stats::qchisq(a, df, lower.tail = FALSE), stats::qchisq(a, df))
  if (any(!is.finite(endpoints))) stop("Variance interval overflow; rescale x.")
  .bc_ci(v, endpoints, conf, "chi-square variance", "Exact for independent normal observations with positive variance.",
    context = c("Sample size" = length(x)), parameter = "variance")
}

#' Sd ci
#'
#' @details Square-root transformation of variance_ci; inherits its normality assumption.
#' @param x Real numeric vector of finite observations.
#' @param conf Confidence level strictly between zero and one.
#' @return A bambagana_interval data frame with estimate, lower, upper, conf,
#' method and assumptions. Printing is concise; as.data.frame returns the
#' numerical table, coef extracts the estimate, and confint extracts stored
#' endpoints without recalculation. median_ci also returns minimum_coverage.
#' @examples
#' sd_ci(1:10)
#' @export
sd_ci <- function(x, conf = 0.95) {
  v <- variance_ci(x, conf); v[c("estimate", "lower", "upper")] <- sqrt(v[c("estimate", "lower", "upper")])
  v$method <- "chi-square SD"; attr(v, "parameter") <- "standard deviation"; v
}

#' Proportion ci
#'
#' @details Wilson score interval without continuity correction, or exact Clopper-Pearson binomial interval. Neither is a Wald interval.
#' @param successes Integer number of successes.
#' @param trials Positive integer number of trials.
#' @param conf Confidence level strictly between zero and one.
#' @param method Method selected from the choices in the function signature.
#' @return A bambagana_interval data frame with estimate, lower, upper, conf,
#' method and assumptions. Printing is concise; as.data.frame returns the
#' numerical table, coef extracts the estimate, and confint extracts stored
#' endpoints without recalculation. median_ci also returns minimum_coverage.
#' @examples
#' proportion_ci(40, 100)
#' @export
proportion_ci <- function(successes, trials, conf = 0.95, method = c("wilson", "exact")) {
  .bc_counts(successes, trials); .bc_conf(conf); method <- match.arg(method)
  interval <- if (method == "exact") stats::binom.test(successes, trials, conf.level = conf)$conf.int else {
    phat <- successes / trials; z <- stats::qnorm((1 - conf) / 2, lower.tail = FALSE)
    den <- 1 + z^2 / trials; center <- (phat + z^2 / (2 * trials)) / den
    half <- z * sqrt(phat * (1 - phat) / trials + z^2 / (4 * trials^2)) / den
    pmax(0, pmin(1, center + c(-half, half)))
  }
  .bc_ci(successes / trials, interval, conf, method,
    "Independent Bernoulli trials with common success probability; Wilson approximate, exact binomial conservative.",
    context = c("Successes" = successes, "Trials" = trials), parameter = "proportion", scale = "percent")
}

#' Mean difference ci
#'
#' @details Difference is mean(x) minus mean(y). Welch is the default; pooled inference requires equal variances.
#' @param x Real numeric vector of finite observations.
#' @param y Second real numeric vector; paired functions match observations by position.
#' @param conf Confidence level strictly between zero and one.
#' @param var_equal Logical; assume equal variances for pooled t inference.
#' @return A bambagana_interval data frame with estimate, lower, upper, conf,
#' method and assumptions. Printing is concise; as.data.frame returns the
#' numerical table, coef extracts the estimate, and confint extracts stored
#' endpoints without recalculation. median_ci also returns minimum_coverage.
#' @examples
#' mean_difference_ci(1:10, 3:14)
#' @export
mean_difference_ci <- function(x, y, conf = 0.95, var_equal = FALSE) {
  x <- .bc_vector(x); y <- .bc_vector(y); .bc_conf(conf); .bc_flag(var_equal)
  a <- stats::t.test(x, y, var.equal = var_equal, conf.level = conf)
  .bc_ci(mean(x) - mean(y), a$conf.int, conf,
    if (var_equal) "pooled t difference" else "Welch t difference",
    "Independent groups; pooled t assumes normality and equal positive variances; Welch uses approximate degrees of freedom.",
    context = c("Group 1 size" = length(x), "Group 2 size" = length(y)),
    parameter = "mean difference (group 1 minus group 2)")
}

#' Paired mean ci
#'
#' @details Pairs are matched by vector position. Differences are x minus y.
#' @param x Real numeric vector of finite observations.
#' @param y Second real numeric vector; paired functions match observations by position.
#' @param conf Confidence level strictly between zero and one.
#' @return A bambagana_interval data frame with estimate, lower, upper, conf,
#' method and assumptions. Printing is concise; as.data.frame returns the
#' numerical table, coef extracts the estimate, and confint extracts stored
#' endpoints without recalculation. median_ci also returns minimum_coverage.
#' @examples
#' paired_mean_ci(1:10, c(2, 4, 3, 6, 5, 8, 7, 10, 9, 12))
#' @export
paired_mean_ci <- function(x, y, conf = 0.95) {
  .bc_pair(x, y); .bc_conf(conf); a <- stats::t.test(x, y, paired = TRUE, conf.level = conf)
  .bc_ci(mean(x - y), a$conf.int, conf, "paired t difference",
    "Independent pairs; exact for normally distributed differences with positive variance.",
    context = c("Number of pairs" = length(x)), parameter = "mean paired difference (x minus y)")
}

#' Proportion difference ci
#'
#' @details Returns the asymptotic two-proportion interval provided by R, with optional continuity correction. Do not describe this difference interval as an exact or Wilson single-proportion interval.
#' @param successes1 Success count for group one.
#' @param trials1 Trial count for group one.
#' @param successes2 Success count for group two.
#' @param trials2 Trial count for group two.
#' @param conf Confidence level strictly between zero and one.
#' @param correct Logical; request continuity correction where applicable.
#' @return A bambagana_interval data frame with estimate, lower, upper, conf,
#' method and assumptions. Printing is concise; as.data.frame returns the
#' numerical table, coef extracts the estimate, and confint extracts stored
#' endpoints without recalculation. median_ci also returns minimum_coverage.
#' @examples
#' proportion_difference_ci(40, 100, 25, 100)
#' @export
proportion_difference_ci <- function(successes1, trials1, successes2, trials2, conf = 0.95, correct = TRUE) {
  .bc_counts(successes1, trials1); .bc_counts(successes2, trials2); .bc_conf(conf); .bc_flag(correct)
  a <- stats::prop.test(c(successes1, successes2), c(trials1, trials2), conf.level = conf, correct = correct)
  .bc_ci(successes1 / trials1 - successes2 / trials2, a$conf.int, conf,
    "R two-proportion difference", "Independent binomial groups; asymptotic interval from stats::prop.test; inspect small-count warnings.",
    context = c("Group 1 successes" = successes1, "Group 1 trials" = trials1,
      "Group 2 successes" = successes2, "Group 2 trials" = trials2),
    parameter = "proportion difference (group 1 minus group 2)", scale = "percentage points")
}

#' Variance ratio ci
#'
#' @details Variance(x) divided by variance(y); exact F inference under independent normal sampling.
#' @param x Real numeric vector of finite observations.
#' @param y Second real numeric vector; paired functions match observations by position.
#' @param conf Confidence level strictly between zero and one.
#' @return A bambagana_interval data frame with estimate, lower, upper, conf,
#' method and assumptions. Printing is concise; as.data.frame returns the
#' numerical table, coef extracts the estimate, and confint extracts stored
#' endpoints without recalculation. median_ci also returns minimum_coverage.
#' @examples
#' variance_ratio_ci(1:10, 3:14)
#' @export
variance_ratio_ci <- function(x, y, conf = 0.95) {
  x <- .bc_vector(x); y <- .bc_vector(y); .bc_conf(conf); a <- stats::var.test(x, y, conf.level = conf)
  .bc_ci(a$estimate, a$conf.int, conf, "F variance ratio", "Independent normal groups with positive variances.",
    context = c("Group 1 size" = length(x), "Group 2 size" = length(y)),
    parameter = "variance ratio (group 1 divided by group 2)")
}

#' Mean test
#'
#' @details One-sample t test. Exact for independent normal observations with positive variance; otherwise approximate.
#' @param x Real numeric vector of finite observations.
#' @param mu Hypothesized mean, location, or difference, as specified in Details.
#' @param alternative Alternative hypothesis accepted by the delegated R test.
#' @param conf Confidence level strictly between zero and one.
#' @return An htest object from R or an explicitly identified chi-square test.
#' @examples
#' mean_test(1:10, mu = 5)
#' @export
mean_test <- function(x, mu = 0, alternative = "two.sided", conf = 0.95) {
  x <- .bc_vector(x); .bc_scalar(mu, "mu"); .bc_conf(conf)
  stats::t.test(x, mu = mu, alternative = alternative, conf.level = conf)
}

#' Paired mean test
#'
#' @details Paired t test of mean(x - y). Exact for independent normally distributed pair differences.
#' @param x Real numeric vector of finite observations.
#' @param y Second real numeric vector; paired functions match observations by position.
#' @param mu Hypothesized mean, location, or difference, as specified in Details.
#' @param alternative Alternative hypothesis accepted by the delegated R test.
#' @param conf Confidence level strictly between zero and one.
#' @return An htest object from R or an explicitly identified chi-square test.
#' @examples
#' paired_mean_test(1:10, c(2, 4, 3, 6, 5, 8, 7, 10, 9, 12))
#' @export
paired_mean_test <- function(x, y, mu = 0, alternative = "two.sided", conf = 0.95) {
  .bc_pair(x, y); .bc_scalar(mu, "mu"); .bc_conf(conf)
  stats::t.test(x, y, paired = TRUE, mu = mu, alternative = alternative, conf.level = conf)
}

#' Two mean test
#'
#' @details Welch two-sample t test by default. Pooled inference assumes equal population variances; groups must be independent.
#' @param x Real numeric vector of finite observations.
#' @param y Second real numeric vector; paired functions match observations by position.
#' @param mu Hypothesized mean, location, or difference, as specified in Details.
#' @param alternative Alternative hypothesis accepted by the delegated R test.
#' @param conf Confidence level strictly between zero and one.
#' @param var_equal Logical; assume equal variances for pooled t inference.
#' @return An htest object from R or an explicitly identified chi-square test.
#' @examples
#' two_mean_test(1:10, 3:14)
#' @export
two_mean_test <- function(x, y, mu = 0, alternative = "two.sided", conf = 0.95, var_equal = FALSE) {
  .bc_vector(x); .bc_vector(y); .bc_scalar(mu, "mu"); .bc_conf(conf); .bc_flag(var_equal)
  stats::t.test(x, y, mu = mu, alternative = alternative, conf.level = conf, var.equal = var_equal)
}

#' Proportion test
#'
#' @details Exact binomial test for independent Bernoulli trials with a common success probability.
#' @param successes Integer number of successes.
#' @param trials Positive integer number of trials.
#' @param p Null success probability strictly between zero and one.
#' @param alternative Alternative hypothesis accepted by the delegated R test.
#' @param conf Confidence level strictly between zero and one.
#' @return An htest object from R or an explicitly identified chi-square test.
#' @examples
#' proportion_test(40, 100)
#' @export
proportion_test <- function(successes, trials, p = 0.5, alternative = "two.sided", conf = 0.95) {
  .bc_counts(successes, trials); .bc_scalar(p, "p", probability = TRUE); .bc_conf(conf)
  stats::binom.test(successes, trials, p = p, alternative = alternative, conf.level = conf)
}

#' Two proportion test
#'
#' @details Asymptotic two-proportion test for independent binomial groups. R warnings for small expected counts are retained.
#' @param successes1 Success count for group one.
#' @param trials1 Trial count for group one.
#' @param successes2 Success count for group two.
#' @param trials2 Trial count for group two.
#' @param alternative Alternative hypothesis accepted by the delegated R test.
#' @param conf Confidence level strictly between zero and one.
#' @param correct Logical; request continuity correction where applicable.
#' @return An htest object from R or an explicitly identified chi-square test.
#' @examples
#' two_proportion_test(40, 100, 25, 100)
#' @export
two_proportion_test <- function(successes1, trials1, successes2, trials2, alternative = "two.sided", conf = 0.95, correct = TRUE) {
  .bc_counts(successes1, trials1); .bc_counts(successes2, trials2); .bc_conf(conf); .bc_flag(correct)
  stats::prop.test(c(successes1, successes2), c(trials1, trials2), alternative = alternative, conf.level = conf, correct = correct)
}

#' Variance test
#'
#' @details Exact chi-square test of a positive population variance under independent normal observations.
#' @param x Real numeric vector of finite observations.
#' @param sigma2 Positive hypothesized population variance.
#' @param alternative Alternative hypothesis accepted by the delegated R test.
#' @return An htest object from R or an explicitly identified chi-square test.
#' @examples
#' variance_test(1:10, sigma2 = 9)
#' @export
variance_test <- function(x, sigma2 = 1, alternative = c("two.sided", "less", "greater")) {
  x <- .bc_vector(x); .bc_scalar(sigma2, "sigma2", positive = TRUE); alternative <- match.arg(alternative)
  df <- length(x) - 1; v <- stats::var(x)
  if (!is.finite(v) || v <= 0) stop("Require positive representable sample variance.")
  statistic <- df * (v / sigma2)
  if (!is.finite(statistic)) stop("Test statistic overflow; rescale x and sigma2.")
  lo <- stats::pchisq(statistic, df); hi <- stats::pchisq(statistic, df, lower.tail = FALSE)
  pv <- switch(alternative, two.sided = min(1, 2 * min(lo, hi)), less = lo, greater = hi)
  structure(list(statistic = c(chisq = statistic), parameter = c(df = df),
    p.value = pv, estimate = c(variance = v), null.value = c(variance = sigma2),
    alternative = alternative, method = "Normal-model chi-square variance test",
    data.name = deparse(substitute(x))), class = "htest")
}

#' Variance ratio test
#'
#' @details Exact F test for a variance ratio under independent normal groups with positive variances.
#' @param x Real numeric vector of finite observations.
#' @param y Second real numeric vector; paired functions match observations by position.
#' @param ratio Positive null variance ratio.
#' @param alternative Alternative hypothesis accepted by the delegated R test.
#' @param conf Confidence level strictly between zero and one.
#' @return An htest object from R or an explicitly identified chi-square test.
#' @examples
#' variance_ratio_test(1:10, 3:14)
#' @export
variance_ratio_test <- function(x, y, ratio = 1, alternative = "two.sided", conf = 0.95) {
  .bc_vector(x); .bc_vector(y); .bc_scalar(ratio, "ratio", positive = TRUE); .bc_conf(conf)
  stats::var.test(x, y, ratio = ratio, alternative = alternative, conf.level = conf)
}

#' Correlation test
#'
#' @details Pearson inference assumes independent bivariate normal pairs; rank tests require independent pairs and their own null model. R tie warnings are retained.
#' @param x Real numeric vector of finite observations.
#' @param y Second real numeric vector; paired functions match observations by position.
#' @param method Method selected from the choices in the function signature.
#' @param alternative Alternative hypothesis accepted by the delegated R test.
#' @param conf Confidence level strictly between zero and one.
#' @return An htest object from R or an explicitly identified chi-square test.
#' @examples
#' correlation_test(1:10, c(2, 4, 3, 6, 5, 8, 7, 10, 9, 12))
#' @export
correlation_test <- function(x, y, method = c("pearson", "spearman", "kendall"), alternative = "two.sided", conf = 0.95) {
  .bc_pair(x, y); .bc_conf(conf); method <- match.arg(method)
  stats::cor.test(x, y, method = method, alternative = alternative, conf.level = conf)
}

#' Chisq independence test
#'
#' @details Chi-square test of independence for count tables from independent units. Sparse-table approximation warnings are retained.
#' @param counts Matrix of nonnegative integer counts with positive row and column margins.
#' @param correct Logical; request continuity correction where applicable.
#' @return An htest object from R or an explicitly identified chi-square test.
#' @examples
#' chisq_independence_test(matrix(c(20, 30, 35, 15), 2))
#' @export
chisq_independence_test <- function(counts, correct = TRUE) {
  .bc_flag(correct)
  if (!is.matrix(counts) || !is.numeric(counts) || is.complex(counts) ||
      any(!is.finite(counts)) || any(counts < 0) || any(counts != floor(counts)) ||
      any(dim(counts) < 2) || any(rowSums(counts) == 0) || any(colSums(counts) == 0))
    stop("Require a nonnegative integer contingency matrix with positive margins.")
  stats::chisq.test(counts, correct = correct)
}

#' Sign test
#'
#' @details Tests equal probabilities of a positive and negative difference among non-tied observations. For a continuous population this tests the median; the returned interval is for the conditional positive-sign probability, not for the median.
#' @param x Real numeric vector of finite observations.
#' @param mu Hypothesized mean, location, or difference, as specified in Details.
#' @param alternative Alternative hypothesis accepted by the delegated R test.
#' @param conf Confidence level strictly between zero and one.
#' @return An htest object from R or an explicitly identified chi-square test.
#' @examples
#' sign_test(1:10, mu = 5)
#' @export
sign_test <- function(x, mu = 0, alternative = "two.sided", conf = 0.95) {
  x <- .bc_vector(x); .bc_scalar(mu, "mu"); .bc_conf(conf); d <- x - mu; d <- d[d != 0]
  if (!length(d)) stop("No nonzero differences remain after removing ties at mu.")
  a <- stats::binom.test(sum(d > 0), length(d), .5, alternative = alternative, conf.level = conf)
  a$method <- "Sign test (ties at mu excluded)"; a
}

#' Wilcoxon one test
#'
#' @details Signed-rank test: independent observations and symmetry about the hypothesized location. It is not a general median test without symmetry.
#' @param x Real numeric vector of finite observations.
#' @param mu Hypothesized mean, location, or difference, as specified in Details.
#' @param alternative Alternative hypothesis accepted by the delegated R test.
#' @param exact NULL for R default, or logical requesting exact rank-test calculations where supported.
#' @return An htest object from R or an explicitly identified chi-square test.
#' @examples
#' wilcoxon_one_test(1:10, mu = 5)
#' @export
wilcoxon_one_test <- function(x, mu = 0, alternative = "two.sided", exact = NULL) {
  .bc_vector(x); .bc_scalar(mu, "mu"); stats::wilcox.test(x, mu = mu, alternative = alternative, exact = exact)
}

#' Wilcoxon two test
#'
#' @details Rank-sum test for independent groups. A location-shift or median interpretation requires additional distributional assumptions, such as common shape.
#' @param x Real numeric vector of finite observations.
#' @param y Second real numeric vector; paired functions match observations by position.
#' @param mu Hypothesized mean, location, or difference, as specified in Details.
#' @param alternative Alternative hypothesis accepted by the delegated R test.
#' @param exact NULL for R default, or logical requesting exact rank-test calculations where supported.
#' @return An htest object from R or an explicitly identified chi-square test.
#' @examples
#' wilcoxon_two_test(1:10, 13:24)
#' @export
wilcoxon_two_test <- function(x, y, mu = 0, alternative = "two.sided", exact = NULL) {
  .bc_vector(x); .bc_vector(y); .bc_scalar(mu, "mu"); stats::wilcox.test(x, y, mu = mu, alternative = alternative, exact = exact)
}

#' Wilcoxon paired test
#'
#' @details Paired signed-rank test: independent pairs with symmetric differences. Ties and zero-difference warnings are retained.
#' @param x Real numeric vector of finite observations.
#' @param y Second real numeric vector; paired functions match observations by position.
#' @param mu Hypothesized mean, location, or difference, as specified in Details.
#' @param alternative Alternative hypothesis accepted by the delegated R test.
#' @param exact NULL for R default, or logical requesting exact rank-test calculations where supported.
#' @return An htest object from R or an explicitly identified chi-square test.
#' @examples
#' wilcoxon_paired_test(1:10, 2 * (1:10))
#' @export
wilcoxon_paired_test <- function(x, y, mu = 0, alternative = "two.sided", exact = NULL) {
  .bc_pair(x, y); .bc_scalar(mu, "mu"); stats::wilcox.test(x, y, paired = TRUE, mu = mu, alternative = alternative, exact = exact)
}

#' Covariance estimate
#'
#' @details Sample covariance, denominator n - 1. Units are the product of the two variable units.
#' @param x Real numeric vector of finite observations.
#' @param y Second real numeric vector; paired functions match observations by position.
#' @return A numeric summary, interval table, matrix, or planning result as described in Details.
#' @examples
#' covariance_estimate(1:10, 2 * (1:10))
#' @export
covariance_estimate <- function(x, y) {
  .bc_pair(x, y); a <- stats::cov(x, y)
  if (!is.finite(a)) stop("Covariance overflow; rescale observations.")
  a
}

#' Correlation estimate
#'
#' @details Pearson linear correlation or Spearman/Kendall rank association. Correlation does not establish causation.
#' @param x Real numeric vector of finite observations.
#' @param y Second real numeric vector; paired functions match observations by position.
#' @param method Method selected from the choices in the function signature.
#' @return A numeric summary, interval table, matrix, or planning result as described in Details.
#' @examples
#' correlation_estimate(1:10, 2 * (1:10))
#' @export
correlation_estimate <- function(x, y, method = c("pearson", "spearman", "kendall")) {
  .bc_pair(x, y); method <- match.arg(method)
  if (stats::sd(x) == 0 || stats::sd(y) == 0) stop("Correlation is undefined for a constant variable.")
  a <- stats::cor(x, y, method = method)
  if (!is.finite(a)) stop("Correlation numerical failure; rescale observations.")
  a
}

#' Correlation ci
#'
#' @details Pearson correlation confidence interval delegated to R. No corresponding rank-correlation confidence interval is silently substituted.
#' @param x Real numeric vector of finite observations.
#' @param y Second real numeric vector; paired functions match observations by position.
#' @param conf Confidence level strictly between zero and one.
#' @return A bambagana_interval data frame with estimate, lower, upper, conf,
#' method and assumptions. Printing is concise; as.data.frame returns the
#' numerical table, coef extracts the estimate, and confint extracts stored
#' endpoints without recalculation. median_ci also returns minimum_coverage.
#' @examples
#' correlation_ci(1:10, c(2, 4, 3, 6, 5, 8, 7, 10, 9, 12))
#' @export
correlation_ci <- function(x, y, conf = 0.95) {
  .bc_pair(x, y); .bc_conf(conf)
  if (length(x) < 4L) stop("Pearson correlation interval requires at least four pairs.")
  a <- stats::cor.test(x, y, conf.level = conf, method = "pearson")
  .bc_ci(a$estimate, a$conf.int, conf, "Fisher-z Pearson correlation",
    "Approximate Fisher-z interval for independent bivariate normal observations.",
    context = c("Number of pairs" = length(x)), parameter = "Pearson correlation")
}

#' Ols fit
#'
#' @details Fits a full-rank ordinary least-squares model using R. Missing data, multivariate responses, rank deficiency, and no residual degrees of freedom are rejected. Classical inference requires an appropriate linear mean model, independent errors, and constant positive variance; normality gives exact finite-sample t/F inference.
#' @param formula Linear-model formula.
#' @param data Data frame containing model variables.
#' @return A fitted R model.
#' @examples
#' ols_fit(mpg ~ wt, mtcars)
#' @export
ols_fit <- function(formula, data) {
  if (!inherits(formula, "formula") || !is.data.frame(data)) stop("Provide a formula and data frame.")
  model <- stats::lm(formula, data = data, na.action = stats::na.fail, x = TRUE, y = TRUE)
  .bc_lm(model)
  if (!is.numeric(model$y) || !is.null(dim(model$y)) || any(!is.finite(model$x)) || any(!is.finite(model$y)))
    stop("Require one finite numeric response and a finite design matrix.")
  model
}

#' Ols coef ci
#'
#' @details Classical coefficient t intervals for a full-rank lm model. These are not heteroscedasticity-robust or clustered intervals.
#' @param model Fitted linear or ANOVA model as appropriate.
#' @param conf Confidence level strictly between zero and one.
#' @return A numeric summary, interval table, matrix, or planning result as described in Details.
#' @examples
#' ols_coef_ci(ols_fit(mpg ~ wt, mtcars))
#' @export
ols_coef_ci <- function(model, conf = 0.95) {
  .bc_lm(model); .bc_conf(conf)
  stats::confint(model, level = conf)
}

#' Ols predict ci
#'
#' @details Confidence intervals estimate the mean response; prediction intervals concern one future observation. Both use the classical linear-model assumptions. Prediction additionally assumes future errors have the fitted model variance.
#' @param model Fitted linear or ANOVA model as appropriate.
#' @param newdata Data frame of predictor values for predictions.
#' @param conf Confidence level strictly between zero and one.
#' @param interval Mean-response confidence interval or future-observation prediction interval.
#' @return A numeric summary, interval table, matrix, or planning result as described in Details.
#' @examples
#' ols_predict_ci(ols_fit(mpg ~ wt, mtcars), data.frame(wt = c(2, 3)))
#' @export
ols_predict_ci <- function(model, newdata, conf = 0.95, interval = c("confidence", "prediction")) {
  .bc_lm(model); .bc_conf(conf); interval <- match.arg(interval)
  if (!is.data.frame(newdata)) stop("newdata must be a data frame.")
  a <- stats::predict(model, newdata = newdata, interval = interval, level = conf, na.action = stats::na.fail)
  if (any(!is.finite(a))) stop("Prediction contains nonfinite results.")
  a
}

#' Anova oneway
#'
#' @details One-way classical ANOVA with independent normal errors and common positive variance. For heterogeneous variances use a suitable alternative rather than treating this as robust ANOVA.
#' @param x Real numeric vector of finite observations.
#' @param group Group labels, one per observation.
#' @return A fitted R model.
#' @examples
#' anova_oneway(c(1:5, 3:7), rep(c("A", "B"), each = 5))
#' @export
anova_oneway <- function(x, group) {
  x <- .bc_vector(x)
  if (length(group) != length(x) || anyNA(group)) stop("Provide one nonmissing group per observation.")
  group <- factor(group)
  if (nlevels(group) < 2L || length(x) <= nlevels(group)) stop("Require at least two groups and positive residual degrees of freedom.")
  stats::aov(response ~ group, data = data.frame(response = x, group = group))
}

#' Anova tukey
#'
#' @details Tukey simultaneous pairwise comparisons using a single-stratum aov model. Inherits ANOVA assumptions; R adjustments for unbalanced designs have limits.
#' @param model Fitted linear or ANOVA model as appropriate.
#' @param conf Confidence level strictly between zero and one.
#' @return A numeric summary, interval table, matrix, or planning result as described in Details.
#' @examples
#' anova_tukey(anova_oneway(c(1:5, 3:7), rep(c("A", "B"), each = 5)))
#' @export
anova_tukey <- function(model, conf = 0.95) {
  .bc_conf(conf)
  if (!inherits(model, "aov") || inherits(model, "aovlist")) stop("Provide a single-stratum aov model.")
  stats::TukeyHSD(model, conf.level = conf)
}

#' Mean power
#'
#' @details Normal-model t-test power from R. For two samples, n is per group and the calculation assumes equal variances and equal group sizes. For paired tests sd is the SD of pair differences. A one-sided positive delta represents the planned rejection direction.
#' @param n Whole number of observations, or observations per group, at least two.
#' @param delta Planned mean difference, in the measurement units.
#' @param sd Positive planning standard deviation; for paired designs use the SD of differences.
#' @param alpha Significance level strictly between zero and one.
#' @param type One-sample, two-sample, or paired t-test design.
#' @param alternative Alternative hypothesis accepted by the delegated R test.
#' @return A numeric summary, interval table, matrix, or planning result as described in Details.
#' @examples
#' mean_power(30, delta = 0.5)
#' @export
mean_power <- function(n, delta, sd = 1, alpha = 0.05, type = c("one.sample", "two.sample", "paired"), alternative = c("two.sided", "one.sided")) {
  .bc_scalar(n, "n", positive = TRUE); .bc_scalar(delta, "delta"); .bc_scalar(sd, "sd", positive = TRUE); .bc_scalar(alpha, "alpha", probability = TRUE)
  if (n < 2 || n != floor(n)) stop("n must be a whole number at least 2.")
  type <- match.arg(type); alternative <- match.arg(alternative)
  stats::power.t.test(n = n, delta = delta, sd = sd, sig.level = alpha,
    type = type, alternative = alternative, strict = TRUE)
}

#' Sample size mean
#'
#' @details Rounds the R t-test sample-size solution up to a whole number and recomputes achieved power. Two-sample n is per group; paired sd describes pair differences. This is model-based planning, not a guarantee under arbitrary data distributions.
#' @param delta Planned mean difference, in the measurement units.
#' @param sd Positive planning standard deviation; for paired designs use the SD of differences.
#' @param power Target power strictly between alpha and one.
#' @param alpha Significance level strictly between zero and one.
#' @param type One-sample, two-sample, or paired t-test design.
#' @param alternative Alternative hypothesis accepted by the delegated R test.
#' @return A numeric summary, interval table, matrix, or planning result as described in Details.
#' @examples
#' sample_size_mean(delta = 0.5)
#' @export
sample_size_mean <- function(delta, sd = 1, power = 0.8, alpha = 0.05, type = c("one.sample", "two.sample", "paired"), alternative = c("two.sided", "one.sided")) {
  .bc_scalar(delta, "delta", positive = TRUE); .bc_scalar(sd, "sd", positive = TRUE)
  .bc_scalar(power, "power", probability = TRUE); .bc_scalar(alpha, "alpha", probability = TRUE)
  if (power <= alpha) stop("Target power must exceed alpha.")
  type <- match.arg(type); alternative <- match.arg(alternative)
  a <- stats::power.t.test(delta = delta, sd = sd, power = power, sig.level = alpha,
    type = type, alternative = alternative, strict = TRUE)
  n <- max(2, ceiling(a$n))
  achieved <- mean_power(n, delta, sd, alpha, type, alternative)$power
  data.frame(n = n, target_power = power, achieved_power = achieved,
    delta = delta, sd = sd, alpha = alpha, type = type, alternative = alternative)
}
