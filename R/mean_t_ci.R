#' Standard t Confidence Interval for a Population Mean
#'
#' Calculates a two-sided t interval using scaled arithmetic.
#' @param x Numeric vector with at least two finite observations.
#' @param conf Confidence level strictly between zero and one.
#' @details Exact coverage requires independent normal observations
#' with a common positive variance. Outside this model, coverage is
#' approximate and may be poor for small or heavy-tailed samples.
#' Constant samples are rejected because studentization is undefined.
#' Scaling protects variance calculations from overflow and underflow;
#' unrepresentable results produce an error.
#' @return A mean_t_ci object containing estimate, lower, upper, standard_error, n,
#' df, conf, method, and assumptions.
#' @examples
#' set.seed(1998)
#' mean_t_ci(rnorm(30, mean = 2, sd = 1))
#' @export
mean_t_ci <- function (x, conf = 0.95) 
{
    if (!is.numeric(x) || is.complex(x) || !is.null(dim(x)) || length(x) < 2 || 
        any(!is.finite(x))) {
        stop("x must contain at least two finite numeric observations.")
    }
    if (!is.numeric(conf) || is.complex(conf) || length(conf) != 1 || !is.finite(conf) || 
        conf <= 0 || conf >= 1) {
        stop("conf must be strictly between 0 and 1.")
    }
    if (all(x == x[1])) {
        stop("The t interval is undefined for a constant sample.")
    }
    center <- min(x)/2 + max(x)/2
    deviations <- x - center
    scale_value <- max(abs(deviations))
    z <- deviations/scale_value
    if (length(unique(z)) != length(unique(x))) {
        stop("Insufficient numeric precision to preserve distinct observations.")
    }
    n <- length(z)
    estimate_scaled <- mean(z)
    se_scaled <- stats::sd(z)/sqrt(n)
    if (!is.finite(se_scaled) || se_scaled <= 0) {
        stop("Insufficient numeric precision to calculate the standard error.")
    }
    critical <- stats::qt((1 - conf)/2, df = n - 1, lower.tail = FALSE)
    endpoints <- (estimate_scaled + c(-1, 1) * critical * se_scaled) * 
        scale_value + center
    estimate <- center + estimate_scaled * scale_value
    standard_error <- se_scaled * scale_value
    if (any(!is.finite(c(estimate, standard_error, endpoints))) || 
        standard_error <= 0 || endpoints[1] >= endpoints[2]) {
        stop("The interval cannot be represented accurately at this scale.")
    }
    structure(list(estimate = estimate, lower = endpoints[1], upper = endpoints[2], 
        standard_error = standard_error, n = n, df = n - 1, conf = conf, 
        method = "standard t", assumptions = paste("Exact coverage for independent normal observations", 
            "with a common positive variance;", "coverage is approximate outside that model.")),
        class = c("mean_t_ci", "mean_ci"))
}
