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

  dots <- list(...)

  details <- if (is.null(dots$details)) {
    FALSE
  } else {
    dots$details
  }

  if (!is.logical(details) ||
      length(details) != 1L ||
      is.na(details)) {
    stop("details must be TRUE or FALSE.")
  }

  if (!is.numeric(digits) ||
      length(digits) != 1L ||
      !is.finite(digits) ||
      digits != floor(digits) ||
      digits < 1L || digits > 15L) {
    stop("digits must be a whole number between 1 and 15.")
  }

  bambagana_show(
    x,
    digits = digits,
    title = "One-sample mean confidence interval"
  )

  if (details) {

    cat("\nTechnical details:\n")

    cat("Degrees of freedom:", x$df, "\n")
    cat("Standard error:",
        signif(x$standard_error, digits), "\n")

    if (!is.null(x$assumptions)) {
      cat("Assumptions:",
          paste(x$assumptions, collapse = " "), "\n")
    }

    cat("Method:", x$method, "\n")
  }

  invisible(x)
}
