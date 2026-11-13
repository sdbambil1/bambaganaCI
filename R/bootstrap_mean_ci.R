#' Bootstrap Confidence Interval for a Population Mean
#'
#' Construct a percentile, basic, normal, or BCa bootstrap interval.
#' @param x Numeric vector with at least two finite observations.
#' @param B Whole number of bootstrap samples, at least 2.
#' @param conf Confidence level strictly between 0 and 1.
#' @param method One of "percentile", "basic", "normal", or "bca".
#' @details Observations must be independent and identically distributed,
#' with finite population mean and variance. The default percentile method
#' uses type-7 quantiles and preserves the original resampling sequence.
#' Extreme magnitudes or large offsets are handled in centered, scaled units.
#' Basic intervals reflect these quantiles about the sample mean. Normal
#' intervals use the bootstrap standard error and estimated bias correction.
#' BCa intervals delegate to boot::boot.ci, with influence values x - mean(x).
#' BCa uses boot's interpolation convention, rather than type-7 quantiles.
#' The non-BCa methods use sample() resampling for backwards compatibility,
#' whereas BCa and compare_mean_ci use boot::boot(). Consequently, the same
#' seed need not generate identical replicates across those entry points.
#' BCa needs at least three nonconstant observations and can be unstable for
#' small samples. Warnings from boot are retained. Set the seed outside this
#' function. Increasing B reduces resampling noise, not sampling bias or
#' failures caused by dependent or unrepresentative observations.
#' Constant samples return a zero-width empirical interval; that does not
#' establish that the population has zero uncertainty.
#' @section Validation and limitations:
#' A study using 10,000 datasets per setting found undercoverage for
#' percentile and BCa intervals under scaled Student t(3) and contaminated
#' normal populations at sample sizes 30 and 100. Basic and normal
#' intervals also showed departures from nominal coverage in some settings.
#' These results do not establish a universally preferable method.
#' Correct implementation and successful computation do not guarantee
#' nominal coverage. See the README for the study settings and results.
#' @return A bootmean_ci object with estimate, lower, upper, bootstrap_means,
#' n, B, conf, method, bootstrap_se, and diagnostics. The original four fields remain.
#' @examples
#' set.seed(1998)
#' result <- bootstrap_mean_ci(c(10, 20, 30, 40, 50))
#' result
#' c(lower = result$lower, upper = result$upper)
#' @export
bootstrap_mean_ci <- function(x, B = 2000, conf = 0.95,
                              method = c("percentile", "basic", "normal", "bca")) {
  if (!is.numeric(x) || is.complex(x) || !is.null(dim(x))) stop("x must be a numeric vector.")
  if (length(x) < 2L || any(!is.finite(x))) {
    stop("x must contain at least two observations, with no NA, NaN, or infinite values.")
  }
  if (!is.numeric(B) || is.complex(B) || length(B) != 1L || !is.finite(B) ||
      B < 2 || B != floor(B) || B > .Machine$integer.max) {
    stop("B must be a whole number between 2 and .Machine$integer.max.")
  }
  if (!is.numeric(conf) || is.complex(conf) || length(conf) != 1L || !is.finite(conf) ||
      conf <= 0 || conf >= 1) {
    stop("conf must be a single number strictly between 0 and 1.")
  }

  method <- match.arg(method)

  # Work in centered units when magnitudes or offsets threaten precision.
  constant_input <- all(x == x[1L])
  magnitude <- max(abs(x))
  center <- min(x) / 2 + max(x) / 2
  deviations <- x - center
  spread <- max(abs(deviations))
  offset_risk <- !constant_input && spread / magnitude < sqrt(.Machine$double.eps)
  if (!offset_risk) {
    center <- 0
    deviations <- x
    spread <- magnitude
  }
  needs_scaling <- !constant_input &&
    (magnitude < 1e-100 || magnitude > 1e100 ||
     offset_risk)
  if (needs_scaling) {
    normalized <- deviations / spread
    if (!is.finite(spread) || spread <= 0 ||
        length(unique(normalized)) != length(unique(x))) {
      stop("Insufficient numeric precision to preserve distinct observations.")
    }
    out <- bootstrap_mean_ci(normalized, B = B, conf = conf, method = method)
    for (field in c("estimate", "lower", "upper", "bootstrap_means")) {
      out[[field]] <- center + out[[field]] * spread
    }
    out$bootstrap_se <- out$bootstrap_se * spread
    values <- unlist(out[c("estimate", "lower", "upper",
                          "bootstrap_means", "bootstrap_se")], use.names = FALSE)
    if (any(!is.finite(values)) || out$bootstrap_se <= 0 || out$lower >= out$upper) {
      stop("Insufficient numeric precision to represent the interval.")
    }
    out$diagnostics$rescaled <- TRUE
    return(out)
  }

  n <- length(x)
  estimate <- mean(x)
  alpha <- 1 - conf
  constant <- all(x == x[1L])
  if (method == "bca" && !constant && n < 3L) {
    stop("BCa requires at least three nonconstant observations.")
  }
  if (!constant && method != "normal" && B * alpha / 2 < 1) {
    warning("Too few resamples in the interval tails; increase B.", call. = FALSE)
  }
  if (method == "bca" && !constant) {
    reference <- boot::boot(x, function(data, indices) mean(data[indices]),
                            R = B, sim = "ordinary", simple = TRUE, parallel = "no")
    bootstrap_means <- as.numeric(reference$t)
  } else {
    bootstrap_means <- numeric(B)
    for (b in seq_len(B)) {
      bootstrap_means[b] <- mean(sample(x, size = n, replace = TRUE))
    }
  }
  if (any(!is.finite(bootstrap_means))) {
    stop("Non-finite resampled means; check the magnitude of x.")
  }
  bootstrap_se <- stats::sd(bootstrap_means)
  if (!is.finite(bootstrap_se)) stop("Non-finite bootstrap standard error; rescale x.")
  if (!constant && bootstrap_se <= 0) {
    stop("Resampled means have no representable variation; increase B or inspect x.")
  }
  if (constant) {
    ci <- rep(estimate, 2)
  } else if (method == "bca") {
    # Scaling influence values prevents overflow without changing acceleration.
    influence <- x - estimate
    influence <- influence / max(abs(influence))
    if (any(!is.finite(influence))) stop("Non-finite influence values; rescale x.")
    bca <- boot::boot.ci(reference, conf = conf, type = "bca", index = 1L,
                         L = influence)
    ci <- as.numeric(bca$bca[1L, 4:5])
  } else if (method == "normal") {
    bias <- mean(bootstrap_means) - estimate
    margin <- stats::qnorm(alpha / 2, lower.tail = FALSE) * bootstrap_se
    ci <- estimate - bias + c(-margin, margin)
  } else {
    quantiles <- stats::quantile(bootstrap_means,
                                probs = c(alpha / 2, 1 - alpha / 2),
                                names = FALSE, type = 7)
    ci <- if (method == "percentile") quantiles else 2 * estimate - rev(quantiles)
  }
  if (length(ci) != 2L || any(!is.finite(ci)) || ci[1L] > ci[2L]) {
    stop("A finite ordered interval could not be computed; increase B or inspect x.")
  }
  if (!constant && ci[1L] == ci[2L]) {
    warning("The empirical interval has zero width for nonconstant data; inspect the resamples.",
            call. = FALSE)
  }
  structure(list(estimate = estimate, lower = ci[1L], upper = ci[2L],
                 bootstrap_means = bootstrap_means, n = n, B = B,
                 conf = conf, method = method, bootstrap_se = bootstrap_se,
                 diagnostics = list(constant_data = constant,
                                    degenerate_interval = ci[1L] == ci[2L],
                                    expected_tail_resamples = B * alpha / 2,
                                    rescaled = FALSE)),
            class = c("bootmean_ci", "mean_ci"))
}

#' Print a Bootstrap Mean Interval
#' @param x A bootmean_ci object.
#' @param digits Number of significant digits to display.
#' @param ... Additional arguments (currently unused).
#' @return Invisibly returns x.
#' @export

print.bootmean_ci <- function(x, digits = 4L, ...) {

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
    title = "Bootstrap mean confidence interval"
  )

  if (isTRUE(x$diagnostics$constant_data)) {
    cat("Constant observed data: zero width does not imply zero population uncertainty.\n")
  }

  if (details) {

    cat("\nTechnical details:\n")

    if (!is.null(x$bootstrap_se)) {
      cat(
        "Bootstrap standard error:",
        signif(x$bootstrap_se, digits),
        "\n"
      )
    }

    if (!is.null(x$diagnostics)) {
      cat("Diagnostics:\n")

      for (name in names(x$diagnostics)) {
        value <- x$diagnostics[[name]]

        cat(
          " ", name, ": ",
          paste(value, collapse = ", "),
          "\n",
          sep = ""
        )
      }
    }

    cat(
      "Bootstrap replicates stored:",
      length(x$bootstrap_means),
      "\n"
    )
  }

  invisible(x)
}
