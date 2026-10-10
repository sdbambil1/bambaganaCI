#' Compare Confidence Intervals for a Population Mean
#'
#' Compare percentile, basic, normal, BCa, and classical t intervals.
#' Bootstrap methods use a common set of resamples.
#' This function uses boot::boot() for all bootstrap methods, whereas
#' bootstrap_mean_ci() preserves sample() resampling for non-BCa methods.
#' Consequently, identical RNG seeds need not yield identical replicates
#' between these two interfaces.
#'
#' @param x Numeric vector of finite observations.
#' @param B Number of bootstrap resamples.
#' @param conf Confidence level between 0 and 1.
#' @param methods Methods to compare.
#' @return A data frame containing interval estimates and widths.
#' @export
compare_mean_ci <- function(
    x, B = 2000, conf = 0.95,
    methods = c('percentile', 'basic', 'normal', 'bca', 't')) {

  allowed <- c('percentile', 'basic', 'normal', 'bca', 't')

  if (!is.character(methods) || !length(methods) ||
      anyNA(methods) || anyDuplicated(methods) ||
      !all(methods %in% allowed)) {
    stop('methods must contain distinct supported method names.')
  }

  if (!is.numeric(x) || is.complex(x) || !is.null(dim(x)) ||
      length(x) < 2L || any(!is.finite(x))) {
    stop('x must contain at least two finite numeric observations.')
  }

  if (!is.numeric(B) || is.complex(B) || length(B) != 1L ||
      !is.finite(B) || B < 2 || B != floor(B) ||
      B > .Machine$integer.max) {
    stop('B must be a whole number of at least 2.')
  }

  if (!is.numeric(conf) || is.complex(conf) ||
      length(conf) != 1L || !is.finite(conf) ||
      conf <= 0 || conf >= 1) {
    stop('conf must be strictly between 0 and 1.')
  }

  bootstrap_methods <- intersect(methods,
    c('percentile', 'basic', 'normal', 'bca'))

  if (any(bootstrap_methods %in% c('percentile', 'basic', 'bca')) &&
      !all(x == x[1L]) && B * (1 - conf) / 2 < 1) {
    warning('Too few resamples in the interval tails;',
            ' increase B.', call. = FALSE)
  }

  if ('bca' %in% methods && length(x) < 3L &&
      !all(x == x[1L])) {
    stop('BCa requires at least three nonconstant observations.')
  }

  if ('t' %in% methods && all(x == x[1L])) {
    stop('The t interval is undefined for a constant sample.')
  }

  # Normalize extreme magnitudes and large offsets.
  constant_input <- all(x == x[1L])
  magnitude <- max(abs(x))
  center <- min(x) / 2 + max(x) / 2
  deviations <- x - center
  spread <- max(abs(deviations))
  offset_risk <- !constant_input &&
    spread / magnitude < sqrt(.Machine$double.eps)

  if (!offset_risk) {
    center <- 0
    spread <- magnitude
  }

  needs_scaling <- !constant_input &&
    (magnitude < 1e-100 || magnitude > 1e100 || offset_risk)

  if (needs_scaling) {
    normalized <- (x - center) / spread
    if (!is.finite(spread) || spread <= 0 ||
        length(unique(normalized)) != length(unique(x))) {
      stop('Insufficient numerical precision.')
    }

    out <- compare_mean_ci(
      normalized, B = B, conf = conf, methods = methods
    )

    out$estimate <- center + out$estimate * spread
    out$lower <- center + out$lower * spread
    out$upper <- center + out$upper * spread
    out$width <- out$upper - out$lower

    values <- unlist(out[c('estimate', 'lower', 'upper', 'width')],
                     use.names = FALSE)
    if (any(!is.finite(values)) || any(out$lower > out$upper) ||
        any(out$width < 0)) {
      stop('Interval cannot be represented accurately.')
    }

    return(out)
  }

  estimate <- mean(x)
  alpha <- 1 - conf
  intervals <- list()

  if (length(bootstrap_methods)) {
    reference <- boot::boot(
      data = x,
      statistic = function(data, indices) mean(data[indices]),
      R = B, sim = 'ordinary', simple = TRUE, parallel = 'no'
    )

    draws <- as.numeric(reference$t)
    se <- stats::sd(draws)

    if (any(!is.finite(draws)) || !is.finite(se)) {
      stop('Non-finite bootstrap results.')
    }

    if (all(x == x[1L])) {
      for (method in bootstrap_methods) {
        intervals[[method]] <- c(estimate, estimate)
      }
    } else {

      if (se <= 0) stop('Bootstrap variation is zero.')

      q <- stats::quantile(draws,
        c(alpha / 2, 1 - alpha / 2),
        names = FALSE, type = 7)

      if ('percentile' %in% methods)
        intervals[['percentile']] <- q

      if ('basic' %in% methods)
        intervals[['basic']] <- 2 * estimate - rev(q)

      if ('normal' %in% methods) {
        bias <- mean(draws) - estimate
        margin <- stats::qnorm(alpha / 2,
          lower.tail = FALSE) * se
        intervals[['normal']] <-
          estimate - bias + c(-margin, margin)
      }

      if ('bca' %in% methods) {
        influence <- x - estimate
        influence <- influence / max(abs(influence))
        ci <- boot::boot.ci(reference, conf = conf,
          type = 'bca', index = 1L, L = influence)
        intervals[['bca']] <- as.numeric(ci$bca[1L, 4:5])
      }
    }
  }

  if ('t' %in% methods) {
    t_result <- mean_t_ci(x, conf = conf)
    intervals[['t']] <- c(t_result$lower, t_result$upper)
  }

  output <- do.call(rbind, lapply(methods, function(method) {
    ci <- intervals[[method]]
    if (length(ci) != 2L || any(!is.finite(ci)) ||
        ci[1L] > ci[2L] || !is.finite(ci[2L] - ci[1L])) {
      stop('A valid interval could not be computed.')
    }
    data.frame(
      method = method, estimate = estimate,
      lower = ci[1L], upper = ci[2L],
      width = ci[2L] - ci[1L],
      conf = conf, n = length(x)
    )
  }))

  rownames(output) <- NULL
  output
}
