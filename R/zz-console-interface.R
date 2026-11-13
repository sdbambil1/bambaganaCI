
.bc_console_label <- function(expr, fallback) {
  if (!is.symbol(expr)) return(fallback)
  label <- gsub('_', ' ', as.character(expr), fixed = TRUE)
  paste0(toupper(substr(label, 1, 1)), substring(label, 2))
}

.bc_console_groups <- function(x, y, first, second) {
  data.frame(
    Group = c(.bc_console_label(first, 'group 1'),
              .bc_console_label(second, 'group 2')),
    Observations = c(length(x), length(y)),
    Average = c(mean(x), mean(y))
  )
}

.bc_original_two_mean_test <- two_mean_test
.bc_original_mean_difference_ci <- mean_difference_ci

two_mean_test <- function(x, y, mu = 0, alternative = 'two.sided',
                          conf = 0.95, var_equal = FALSE) {
  result <- .bc_original_two_mean_test(
    x, y, mu, alternative, conf, var_equal
  )
  attr(result, 'console_groups') <- .bc_console_groups(
    x, y, substitute(x), substitute(y)
  )
  class(result) <- c('bambagana_comparison_test', class(result))
  result
}

mean_difference_ci <- function(x, y, conf = 0.95, var_equal = FALSE) {
  result <- .bc_original_mean_difference_ci(x, y, conf, var_equal)
  attr(result, 'console_groups') <- .bc_console_groups(
    x, y, substitute(x), substitute(y)
  )
  class(result) <- c('bambagana_comparison_ci', class(result))
  result
}

.bc_console_print <- function(x, digits, test) {
  if (!is.numeric(digits) || is.complex(digits) ||
      length(digits) != 1L || !is.finite(digits) ||
      digits < 1 || digits > 22 || digits != floor(digits))
    stop('digits must be a whole number between 1 and 22.')

  groups <- attr(x, 'console_groups')
  fmt <- function(v) format(v, digits = digits, trim = TRUE)
  display <- groups
  display$Average <- fmt(groups$Average)

  cat('COMPARISON OF MEANS\n\n')
  print(display, row.names = FALSE)

  cat('\nDifference (', groups$Group[1], ' minus ',
      groups$Group[2], '): ',
      fmt(groups$Average[1] - groups$Average[2]), '\n', sep = '')

  endpoints <- if (test) x$conf.int else c(x$lower, x$upper)
  conf <- if (test) attr(x$conf.int, 'conf.level') else x$conf

  cat(fmt(100 * conf), '% confidence interval: [',
      fmt(endpoints[1]), ', ', fmt(endpoints[2]), ']\n', sep = '')

  if (test) {
    cat('P-value: ', format.pval(x$p.value, digits = digits),
        '\n', sep = '')
    cat('Null difference: ', fmt(x$null.value),
        ' | Alternative: ', x$alternative, '\n', sep = '')

    if (is.finite(x$p.value)) {
      cat(if (x$p.value < .05)
        'Interpretation: Evidence against the stated null hypothesis at the 5% level.\n'
      else
        'Interpretation: Insufficient evidence to reject the stated null hypothesis at the 5% level.\n')
    }
  }

  cat('Method: ', x$method, '\n', sep = '')
  cat('For technical details, use print(result, details = TRUE).\n')
  invisible(x)
}

print.bambagana_comparison_test <- function(x, digits = 4L, details = FALSE, ...) {
    if (!is.logical(details) || length(details) != 1L || is.na(details)) 
        stop("details must be TRUE or FALSE.")
    if (!isTRUE(details)) {
        bambagana_show(x, digits = digits)
        return(invisible(x))
    }
    if (!is.logical(details) || length(details) != 1L || is.na(details)) 
        stop("details must be TRUE or FALSE.")
    if (details) {
        NextMethod("print", digits = digits, ...)
        return(invisible(x))
    }
    .bc_console_print(x, digits, TRUE)
}

print.bambagana_comparison_ci <- function(x, digits = 4L, details = FALSE, ...) {
    if (!is.logical(details) || length(details) != 1L || is.na(details)) 
        stop("details must be TRUE or FALSE.")
    if (FALSE) {
        bambagana_show(x, digits = digits)
        return(invisible(x))
    }
    if (!is.logical(details) || length(details) != 1L || is.na(details)) 
        stop("details must be TRUE or FALSE.")
    if (details) {
        NextMethod("print", digits = digits, ...)
        return(invisible(x))
    }
    .bc_console_print(x, digits, FALSE)
}

