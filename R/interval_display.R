#' Display a confidence interval
#' @param x A bambagana_interval result.
#' @param digits Significant digits for the displayed values, at least 1.
#' @param ... Additional arguments, reserved for generic compatibility.
#' @return x invisibly. No stored values are changed.
#' @export
print.bambagana_interval <- function(x, digits = 4L, ...) {
    details <- list(...)$details
    if (is.null(details)) 
        details <- FALSE
    if (!is.logical(details) || length(details) != 1L || is.na(details)) 
        stop("details must be TRUE or FALSE.")
    if (FALSE) {
        bambagana_show(x, digits = digits)
        return(invisible(x))
    }
    if (!is.numeric(digits) || length(digits) != 1L || !is.finite(digits) || digits < 1 || digits > 22 || digits != floor(digits)) 
        stop("digits must be a whole number between 1 and 22.")
    if (nrow(x) != 1L || !all(c("estimate", "lower", "upper", "conf", "method", "assumptions") %in% names(x))) {
        print(as.data.frame(x), digits = digits, ...)
        return(invisible(x))
    }
    method_labels <- c(wilson = "Wilson", exact = "Exact binomial", `known-sigma z` = "Known-sigma z", `order-statistic median` = "Order-statistic", 
        `chi-square variance` = "Chi-square", `chi-square SD` = "Chi-square", `Welch t difference` = "Welch t", `pooled t difference` = "Pooled t", 
        `paired t difference` = "Paired t", `R two-proportion difference` = "Asymptotic two-proportion", `F variance ratio` = "F", 
        `Fisher-z Pearson correlation` = "Fisher-z")
    method <- unname(method_labels[x$method])
    if (is.na(method)) 
        method <- x$method
    parameter <- attr(x, "parameter")
    if (is.null(parameter)) 
        parameter <- "estimate"
    cat(format(100 * x$conf, trim = TRUE), "% ", method, " confidence interval for ", parameter, "\n", sep = "")
    context <- attr(x, "context")
    if (length(context)) 
        cat(paste(paste(names(context), context, sep = ": "), collapse = " | "), "\n")
    scale <- attr(x, "display_scale")
    percent <- identical(scale, "percent")
    points <- identical(scale, "percentage points")
    factor <- if (percent || points) 
        100
    else 1
    suffix <- if (percent) 
        "%"
    else if (points) 
        " percentage points"
    else ""
    display <- function(value) paste0(format(value * factor, digits = digits, trim = TRUE), suffix)
    cat("Estimate: ", display(x$estimate), "\n", sep = "")
    cat("Interval: [", display(x$lower), ", ", display(x$upper), "]\n", sep = "")
    if ("minimum_coverage" %in% names(x)) 
        cat("Continuous-model minimum coverage: ", format(100 * x$minimum_coverage, digits = digits, trim = TRUE), "%\n", 
            sep = "")
    cat("Assumptions: ", x$assumptions, "\n", sep = "")
    invisible(x)
}

#' Extract the confidence-interval table
#' @param x A bambagana_interval result.
#' @param row.names Optional row names for the returned table.
#' @param optional Logical; included for generic compatibility.
#' @param ... Additional arguments, reserved for generic compatibility.
#' @return A plain data frame with original numerical values and named columns.
#' @export
as.data.frame.bambagana_interval <- function(x, row.names = NULL, optional = FALSE, ...) {
  class(x) <- 'data.frame'
  attr(x, 'context') <- NULL; attr(x, 'parameter') <- NULL; attr(x, 'display_scale') <- NULL
  if (!is.null(row.names)) rownames(x) <- row.names
  x
}

#' Extract the interval estimate
#' @param object A bambagana_interval result.
#' @param ... Additional arguments, reserved for generic compatibility.
#' @return A named numeric vector of estimates.
#' @export
coef.bambagana_interval <- function(object, ...) {
  if (nrow(object) != 1L) stop('Extract estimates from a single interval result.')
  value <- object$estimate
  names(value) <- attr(object, 'parameter')
  value
}

#' Extract stored confidence limits
#' @param object A bambagana_interval result.
#' @param parm NULL, 1, or the parameter label displayed in coef(object).
#' @param level The stored confidence level. A different level is rejected.
#' @param ... Additional arguments, reserved for generic compatibility.
#' @return A matrix with lower and upper columns. This does not rerun the analysis.
#' @export
confint.bambagana_interval <- function(object, parm = NULL, level = object$conf, ...) {
  if (nrow(object) != 1L) stop('Extract limits from a single interval result.')
  .bc_conf(level)
  if (!isTRUE(all.equal(level, object$conf, tolerance = 0)))
    stop('This result stores a different confidence level. Rerun the original function with conf = level.')
  parameter <- attr(object, 'parameter')
  if (!is.null(parm) && !identical(parm, parameter) && !identical(parm, 1L) && !identical(parm, 1))
    stop('parm must identify the single stored parameter.')
  matrix(c(object$lower, object$upper), nrow = 1,
         dimnames = list(parameter, c('lower', 'upper')))
}
