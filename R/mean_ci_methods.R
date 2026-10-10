#' Extract and Display Mean Confidence Intervals
#'
#' Common extractors for bootstrap and standard t interval results.
#' @param object A result returned by bootstrap_mean_ci or mean_t_ci.
#' @param x A result returned by mean_t_ci, or either result for as.data.frame.
#' @param parm Either "mean" or the integer 1.
#' @param level The confidence level used when constructing object.
#' @param row.names Optional row names for the summary data frame.
#' @param optional Passed to data.frame.
#' @param digits Number of significant digits in printed output.
#' @param ... Additional arguments (currently unused).
#' @details confint extracts the stored interval. It rejects a different level;
#' rerun the original function with conf set to that level. It never silently
#' relabels an interval or generates fresh resamples. coef returns the sample
#' mean, and as.data.frame returns one summary row without bootstrap replicates.
#' @return coef returns a named numeric estimate; confint a one-row matrix;
#' as.data.frame a one-row data frame; print invisibly returns x.
#' @examples
#' set.seed(1998)
#' out <- bootstrap_mean_ci(1:10)
#' coef(out)
#' confint(out)
#' as.data.frame(out)
#' @name mean_ci_methods
NULL

#' @rdname mean_ci_methods
#' @export
coef.mean_ci <- function(object, ...) {
  c(mean = object$estimate)
}

#' @rdname mean_ci_methods
#' @export
confint.mean_ci <- function(object, parm = "mean", level = object$conf, ...) {
  if (length(parm) != 1L || is.na(parm) ||
      !(identical(parm, "mean") || (is.numeric(parm) && parm == 1))) {
    stop("parm must be 'mean' or 1.")
  }
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
      level != object$conf) {
    stop("level must equal the stored confidence level; rerun the original function with conf = level.")
  }
  matrix(c(object$lower, object$upper), nrow = 1L,
         dimnames = list("mean", c("lower", "upper")))
}

#' @rdname mean_ci_methods
#' @export
as.data.frame.mean_ci <- function(x, row.names = NULL, optional = FALSE, ...) {
  se <- if (inherits(x, "bootmean_ci")) x$bootstrap_se else x$standard_error
  data.frame(estimate = x$estimate, lower = x$lower, upper = x$upper,
             standard_error = se, n = x$n, conf = x$conf, method = x$method,
             row.names = row.names, check.names = !optional)
}

#' @rdname mean_ci_methods
#' @export
print.mean_t_ci <- function(x, digits = 4L, ...) {
  if (!is.numeric(digits) || length(digits) != 1L ||
      !is.finite(digits) || digits != floor(digits) ||
      digits < 1L || digits > 22L) {
    stop("digits must be a single whole number from 1 to 22.")
  }
  cat(sprintf("%g%% standard t confidence interval for the mean\n", 100 * x$conf))
  cat("Sample size:", x$n, " | Degrees of freedom:", x$df, "\n")
  cat("Estimate:", format(signif(x$estimate, digits)), "\n")
  cat("Interval: [", format(signif(x$lower, digits)), ", ",
      format(signif(x$upper, digits)), "]\n", sep = "")
  cat("Standard error:", format(signif(x$standard_error, digits)), "\n")
  invisible(x)
}
