
# bambaganaCI simple input interface v1
# Usability revision: contextual errors and automatic display.

#' Simple Statistical Analysis Interface
#'
#' Run common analyses using readable task names.
#' The original statistical functions perform all calculations.
#'
#' By default, results are displayed in the console and returned
#' invisibly. Set display = FALSE to suppress printing and obtain
#' a visible return value.
#'
#' @param task Name of the requested analysis.
#' @param x First data vector, count pair, or formula.
#' @param y Optional second data vector or count pair.
#' @param ... Additional named options.
#' @param conf Confidence level between zero and one.
#' @param display Whether to display a formatted result.
#'
#' @return The original statistical result, without alteration.
#' @export
bambagana <- function(task, x = NULL, y = NULL, ...,
                      conf = 0.95, display = TRUE) {

  available <- c(
    "describe",
    "mean",
    "compare means",
    "paired",
    "proportion",
    "compare proportions",
    "bootstrap",
    "test mean",
    "test two means",
    "regression",
    "anova",
    "correlation"
  )

  fail <- function(message, example = NULL) {
    if (!is.null(example)) {
      message <- paste0(
        message,
        "\nExample: ",
        example
      )
    }
    stop(message, call. = FALSE)
  }

  if (!is.character(task) ||
      length(task) != 1L ||
      is.na(task) ||
      !nzchar(trimws(task))) {
    fail(
      "Provide one analysis name.",
      "bambagana('mean', scores)"
    )
  }

  task <- tolower(trimws(task))
  task <- gsub("[_-]+", " ", task)
  task <- gsub("[[:space:]]+", " ", task)

  aliases <- c(
    "descriptive statistics" = "describe",
    "descriptives" = "describe",
    "summary" = "describe",
    "mean ci" = "mean",
    "two means" = "compare means",
    "mean difference" = "compare means",
    "paired mean" = "paired",
    "paired ci" = "paired",
    "proportion ci" = "proportion",
    "two proportions" = "compare proportions",
    "proportion difference" = "compare proportions",
    "bootstrap mean" = "bootstrap",
    "mean test" = "test mean",
    "one sample test" = "test mean",
    "two mean test" = "test two means",
    "two sample test" = "test two means",
    "linear regression" = "regression",
    "one way anova" = "anova",
    "cor" = "correlation"
  )

  if (task %in% names(aliases)) {
    task <- unname(aliases[[task]])
  }

  if (task %in% c("help", "methods")) {

    cat("\nBAMBAGANACI - AVAILABLE ANALYSES\n")
    cat("--------------------------------\n")

    examples <- c(
      "describe" =
        "bambagana('describe', scores)",
      "mean" =
        "bambagana('mean', scores)",
      "compare means" =
        "bambagana('compare means', group_A, group_B)",
      "paired" =
        "bambagana('paired', before, after)",
      "proportion" =
        "bambagana('proportion', c(65, 100))",
      "compare proportions" =
        paste0(
          "bambagana('compare proportions', ",
          "c(65, 100), c(45, 100))"
        ),
      "bootstrap" =
        "bambagana('bootstrap', scores, B = 999)",
      "test mean" =
        "bambagana('test mean', scores, mu = 70)",
      "test two means" =
        "bambagana('test two means', group_A, group_B)",
      "regression" =
        "bambagana('regression', salary ~ experience, data = df)",
      "anova" =
        "bambagana('anova', scores, treatment)",
      "correlation" =
        "bambagana('correlation', group_A, group_B)"
    )

    for (name in names(examples)) {
      cat(
        sprintf("\n%-21s %s\n", name, examples[[name]])
      )
    }

    cat(
      "\nFor proportions, c(65, 100) means ",
      "65 successes out of 100 trials.\n",
      sep = ""
    )

    cat(
      "Use display = FALSE to suppress console output.\n"
    )

    return(invisible(available))
  }

  if (!(task %in% available)) {
    fail(
      paste0(
        "Unknown analysis: '", task, "'. ",
        "Run bambagana('methods') to see available analyses."
      ),
      "bambagana('mean', scores)"
    )
  }

  if (is.null(x)) {
    fail(
      paste0("Missing input data for '", task, "'."),
      switch(
        task,
        "regression" =
          "bambagana('regression', y ~ x, data = df)",
        "proportion" =
          "bambagana('proportion', c(65, 100))",
        "compare proportions" =
          paste0(
            "bambagana('compare proportions', ",
            "c(65, 100), c(45, 100))"
          ),
        "bambagana('mean', scores)"
      )
    )
  }

  if (!is.numeric(conf) ||
      length(conf) != 1L ||
      !is.finite(conf) ||
      conf <= 0 || conf >= 1) {
    fail(
      "conf must be a single number between 0 and 1.",
      "bambagana('mean', scores, conf = 0.99)"
    )
  }

  if (!is.logical(display) ||
      length(display) != 1L ||
      is.na(display)) {
    fail(
      "display must be TRUE or FALSE.",
      "bambagana('mean', scores, display = FALSE)"
    )
  }

  extra <- list(...)

  if (length(extra)) {
    nm <- names(extra)

    if (is.null(nm) ||
        anyNA(nm) ||
        any(!nzchar(nm)) ||
        anyDuplicated(nm)) {
      fail(
        "Additional options must have unique names.",
        "bambagana('bootstrap', scores, B = 999)"
      )
    }
  }

  allowed <- switch(
    task,
    "describe" = character(),
    "mean" = character(),
    "compare means" = "var_equal",
    "paired" = character(),
    "proportion" = "method",
    "compare proportions" = "correct",
    "bootstrap" = c("B", "method"),
    "test mean" = c("mu", "alternative"),
    "test two means" = c("mu", "alternative", "var_equal"),
    "regression" = "data",
    "anova" = character(),
    "correlation" = "method"
  )

  unknown <- setdiff(names(extra), allowed)

  if (length(unknown)) {
    message <- paste0(
      "Unsupported option for '", task, "': ",
      paste(unknown, collapse = ", "), "."
    )

    if (length(allowed)) {
      message <- paste0(
        message,
        "\nAllowed options: ",
        paste(allowed, collapse = ", "), "."
      )
    } else {
      message <- paste0(
        message,
        "\nThis analysis has no additional options."
      )
    }

    fail(message)
  }

  count_pair <- function(z, label) {

    valid <- is.numeric(z) &&
      length(z) == 2L &&
      all(is.finite(z)) &&
      all(z == floor(z)) &&
      z[1L] >= 0 &&
      z[2L] > 0 &&
      z[1L] <= z[2L]

    if (!valid) {
      fail(
        paste0(
          label,
          " must contain successes followed by trials. ",
          "Successes cannot exceed trials."
        ),
        "c(65, 100)"
      )
    }

    unname(z)
  }

  second_group <- function() {

    if (is.null(y)) {

      example <- switch(
        task,
        "compare means" =
          "bambagana('compare means', group_A, group_B)",
        "paired" =
          "bambagana('paired', before, after)",
        "compare proportions" =
          paste0(
            "bambagana('compare proportions', ",
            "c(65, 100), c(45, 100))"
          ),
        "test two means" =
          "bambagana('test two means', group_A, group_B)",
        "anova" =
          "bambagana('anova', scores, treatment)",
        "correlation" =
          "bambagana('correlation', group_A, group_B)"
      )

      fail("This analysis requires two inputs.", example)
    }
  }

  if (task %in% c("describe", "mean", "bootstrap", "test mean") &&
      !is.null(y)) {
    fail(
      "This analysis uses one data vector; remove the second input."
    )
  }

  # All calculations are delegated to existing functions.

  result <- switch(
    task,

    "describe" = describe_numeric(x),

    "mean" = do.call(
      mean_t_ci,
      c(list(x = x, conf = conf), extra)
    ),

    "compare means" = {
      second_group()
      do.call(
        mean_difference_ci,
        c(list(x = x, y = y, conf = conf), extra)
      )
    },

    "paired" = {
      second_group()
      if (length(x) != length(y)) {
        fail(
          "Paired samples must have the same length.",
          "bambagana('paired', before, after)"
        )
      }
      do.call(
        paired_mean_ci,
        c(list(x = x, y = y, conf = conf), extra)
      )
    },

    "proportion" = {
      if (!is.null(y)) {
        fail(
          "A single proportion needs only one count pair.",
          "bambagana('proportion', c(65, 100))"
        )
      }

      counts <- count_pair(x, "Proportion input")

      do.call(
        proportion_ci,
        c(
          list(
            successes = counts[1L],
            trials = counts[2L],
            conf = conf
          ),
          extra
        )
      )
    },

    "compare proportions" = {
      second_group()

      a <- count_pair(x, "Group 1")
      b <- count_pair(y, "Group 2")

      do.call(
        proportion_difference_ci,
        c(
          list(
            successes1 = a[1L],
            trials1 = a[2L],
            successes2 = b[1L],
            trials2 = b[2L],
            conf = conf
          ),
          extra
        )
      )
    },

    "bootstrap" = do.call(
      bootstrap_mean_ci,
      c(list(x = x, conf = conf), extra)
    ),

    "test mean" = do.call(
      mean_test,
      c(list(x = x, conf = conf), extra)
    ),

    "test two means" = {
      second_group()
      do.call(
        two_mean_test,
        c(list(x = x, y = y, conf = conf), extra)
      )
    },

    "regression" = {
      if (!inherits(x, "formula")) {
        fail(
          "Regression requires a formula.",
          "bambagana('regression', salary ~ experience, data = df)"
        )
      }

      if (!("data" %in% names(extra))) {
        fail(
          "Provide the data frame with data =.",
          "bambagana('regression', salary ~ experience, data = df)"
        )
      }

      if (!is.null(y)) {
        fail(
          "Regression uses a formula and data =, not a second vector."
        )
      }

      do.call(
        ols_fit,
        c(list(formula = x), extra)
      )
    },

    "anova" = {
      second_group()
      anova_oneway(x, y)
    },

    "correlation" = {
      second_group()
      do.call(
        correlation_estimate,
        c(list(x = x, y = y), extra)
      )
    }
  )



  # bambaganaCI v0.3.2 interface improvements
  # All formatting operates on completed statistical results.

  display_number <- function(value, digits = 5L) {

    if (length(value) == 0L) return("NA")

    value <- value[1L]

    if (is.numeric(value) && is.finite(value)) {
      return(
        format(
          signif(value, digits),
          scientific = FALSE,
          trim = TRUE
        )
      )
    }

    as.character(value)
  }

  display_row <- function(label, value) {
    cat(sprintf("%-25s %s\n", paste0(label, ":"), value))
  }

  display_descriptives <- function(z) {

    labels <- c(
      n = "Observations",
      mean = "Mean",
      median = "Median",
      sd = "Standard deviation",
      variance = "Variance",
      minimum = "Minimum",
      maximum = "Maximum",
      q1 = "First quartile",
      q3 = "Third quartile"
    )

    cat("\nDESCRIPTIVE STATISTICS\n")
    cat("----------------------\n")

    for (key in names(labels)) {

      if (key %in% names(z)) {

        display_row(
          labels[[key]],
          display_number(z[[key]])
        )
      }
    }

    invisible(z)
  }

  display_proportions <- function(z, task, x, y, conf) {

    pct <- function(value) {
      display_number(100 * as.numeric(value))
    }

    if (task == "proportion") {

      cat("\nPROPORTION CONFIDENCE INTERVAL\n")
      cat("------------------------------\n")

      display_row(
        "Successes / trials",
        paste0(x[1L], " / ", x[2L])
      )

      display_row(
        "Estimated proportion",
        paste0(pct(z$estimate), "%")
      )

      display_row(
        paste0(display_number(conf * 100), "% confidence interval"),
        paste0(
          "[", pct(z$lower), "%, ",
          pct(z$upper), "%]"
        )
      )

    } else {

      cat("\nDIFFERENCE IN PROPORTIONS\n")
      cat("-------------------------\n")

      display_row(
        "Group 1",
        paste0(
          x[1L], " / ", x[2L],
          " (", pct(x[1L] / x[2L]), "%)"
        )
      )

      display_row(
        "Group 2",
        paste0(
          y[1L], " / ", y[2L],
          " (", pct(y[1L] / y[2L]), "%)"
        )
      )

      display_row(
        "Estimated difference",
        paste0(pct(z$estimate), " percentage points")
      )

      display_row(
        paste0(display_number(conf * 100), "% confidence interval"),
        paste0(
          "[", pct(z$lower), ", ",
          pct(z$upper), "] percentage points"
        )
      )
    }

    if ("method" %in% names(z)) {
      display_row(
        "Method",
        as.character(z$method[1L])
      )
    }

    if ("assumptions" %in% names(z)) {

      assumptions <- as.character(z$assumptions[1L])

      if (length(assumptions) &&
          !is.na(assumptions) &&
          nzchar(assumptions)) {
        cat("\nAssumptions:\n")
        cat(assumptions, "\n")
      }
    }

    invisible(z)
  }

  display_mean_comparison_interpretation <- function(z, conf) {

    estimates <- as.numeric(z$estimate)

    if (length(estimates) < 2L ||
        any(!is.finite(estimates[1:2]))) {
      return(invisible(z))
    }

    difference <- estimates[1L] - estimates[2L]
    magnitude <- display_number(abs(difference))

    cat("\nInterpretation:\n")

    if (difference > 0) {

      cat(
        "Group 1 has a sample mean ",
        magnitude,
        " units higher than Group 2.\n",
        sep = ""
      )

    } else if (difference < 0) {

      cat(
        "Group 2 has a sample mean ",
        magnitude,
        " units higher than Group 1.\n",
        sep = ""
      )

    } else {

      cat("The two groups have equal sample means.\n")
    }

    alpha <- 1 - conf
    p <- as.numeric(z$p.value)[1L]

    if (length(p) != 1L || !is.finite(p)) {
      return(invisible(z))
    }

    if (identical(z$alternative, "two.sided")) {

      if (p < alpha) {

        cat(
          "The test provides evidence that the ",
          "population means differ.\n",
          sep = ""
        )

      } else {

        cat(
          "The test does not provide sufficient ",
          "evidence that the population means differ.\n",
          sep = ""
        )
      }

    } else {

      cat(
        "The statistical conclusion concerns the ",
        "specified one-sided alternative.\n",
        sep = ""
      )
    }

    invisible(z)
  }


  # Return the original statistical object.
  # Formatting happens independently of calculations.

  if (!display) {
    return(result)
  }

  if (task == "describe") {

    display_descriptives(result)

  } else if (task %in% c("proportion", "compare proportions")) {

    display_proportions(result, task, x, y, conf)

  } else if (task %in% c(
    "test mean", "regression", "anova", "correlation"
  )) {

    display_title <- if (task == "correlation") {
      "CORRELATION"
    } else {
      NULL
    }

    bambagana_show(
      result,
      title = display_title
    )

  } else {

    # Preserve specialized count, percentage-point,
    # comparison, and bootstrap displays.
    print(result)
  }

  if (task == "test two means") {
    display_mean_comparison_interpretation(result, conf)
  }

  invisible(result)
}




# BEGIN BAMBAGANA COMPLETE FRIENDLY INTERFACE

# Preserve the validated interface and its existing behavior.
bambagana <- local({

  original_bambagana <- bambagana

  normalize_task <- function(task) {

    task <- tolower(trimws(task))

    task <- gsub("_", " ", task, fixed = TRUE)
    task <- gsub("-", " ", task, fixed = TRUE)
    task <- gsub("[[:space:]]+", " ", task)

    trimws(task)
  }

  friendly_function_names <- function() {

    exports <- getNamespaceExports("bambaganaCI")

    statistical <- sort(
      setdiff(
        exports,
        c(
          "bambagana",
          "bambagana_show",
          "bambagana_catalog"
        )
      )
    )

    aliases <- gsub("_", " ", statistical, fixed = TRUE)

    names(statistical) <- aliases

    statistical
  }

  special_aliases <- c(
    "confidence interval" = "mean",
    "confidence intervals" = "mean",
    "ci" = "mean",
    "mean confidence interval" = "mean",
    "confidence interval for mean" = "mean",
    "confidence interval for a mean" = "mean",
    "average confidence interval" = "mean",

    "descriptive statistics" = "describe",
    "descriptives" = "describe",
    "summary statistics" = "describe",
    "describe data" = "describe",

    "standard error" = "mean_se",
    "standard error of mean" = "mean_se",

    "compare averages" = "compare means",
    "difference between means" = "compare means",
    "difference in means" = "compare means",
    "confidence interval for two means" = "compare means",

    "paired confidence interval" = "paired",
    "paired means" = "paired",

    "confidence interval for proportion" = "proportion",
    "proportion confidence interval" = "proportion",

    "difference between proportions" = "compare proportions",
    "difference in proportions" = "compare proportions",
    "confidence interval for two proportions" =
      "compare proportions",

    "bootstrap confidence interval" = "bootstrap",
    "bootstrap interval" = "bootstrap",

    "hypothesis test" = "test mean",
    "one sample t test" = "test mean",
    "test average" = "test mean",

    "two sample t test" = "test two means",
    "compare mean test" = "test two means",

    "linear regression" = "regression",
    "regression analysis" = "regression",

    "analysis of variance" = "anova",
    "one way anova" = "anova",

    "pearson correlation" = "correlation"
  )

  original_commands <- c(
    "describe",
    "mean",
    "compare means",
    "paired",
    "proportion",
    "compare proportions",
    "bootstrap",
    "test mean",
    "test two means",
    "regression",
    "anova",
    "correlation",
    "methods",
    "help"
  )

  function(task, ..., display = TRUE) {

    if (!is.character(task) ||
        length(task) != 1L ||
        is.na(task) ||
        !nzchar(trimws(task))) {

      stop(
        "Enter an analysis name, for example ",
        "bambagana('confidence interval', scores).",
        call. = FALSE
      )
    }

    if (!is.logical(display) ||
        length(display) != 1L ||
        is.na(display)) {

      stop(
        "display must be TRUE or FALSE.",
        call. = FALSE
      )
    }

    requested <- normalize_task(task)
    function_map <- friendly_function_names()

    # --------------------------------------------------------
    # Show all available statistical functions.
    # --------------------------------------------------------

    if (requested %in% c(
      "all functions",
      "functions",
      "function list",
      "all analyses",
      "full catalog"
    )) {

      technical_names <- unname(function_map)

      signatures <- vapply(
        technical_names,
        function(name) {

          fun <- getExportedValue(
            "bambaganaCI", name
          )

          arguments <- names(formals(fun))

          if (length(arguments) == 0L) {
            arguments <- character()
          }

          paste0(
            name, "(",
            paste(arguments, collapse = ", "),
            ")"
          )
        },
        character(1)
      )

      inventory <- data.frame(
        Command = names(function_map),
        R_function = technical_names,
        Arguments = unname(signatures),
        stringsAsFactors = FALSE
      )

      if (display) {

        cat("\nBAMBAGANACI - ALL STATISTICAL FUNCTIONS\n")
        cat("--------------------------------------\n")
        cat(
          "Use the readable command in bambagana().\n",
          "Additional arguments depend on the function.\n\n",
          sep = ""
        )

        print(
          inventory,
          row.names = FALSE,
          right = FALSE
        )

        cat(
          "\nExample:\n",
          "bambagana('confidence interval', scores)\n\n",
          "To suppress output, use display = FALSE.\n",
          sep = ""
        )
      }

      return(invisible(inventory))
    }

    # --------------------------------------------------------
    # Check predefined beginner-friendly aliases.
    # --------------------------------------------------------

    if (requested %in% names(special_aliases)) {

      requested <- normalize_task(
        unname(special_aliases[[requested]])
      )
    }

    # --------------------------------------------------------
    # Preserve existing validated commands.
    # --------------------------------------------------------

    if (requested %in% original_commands) {

      return(
        original_bambagana(
          requested,
          ...,
          display = display
        )
      )
    }

    # --------------------------------------------------------
    # Route every statistical export through bambagana().
    # --------------------------------------------------------

    if (requested %in% names(function_map)) {

      technical_name <- unname(
        function_map[[requested]]
      )

      statistical_function <- getExportedValue(
        "bambaganaCI",
        technical_name
      )

      # Forward arguments without modifying their meaning,
      # order, defaults, or numerical computation.
      result <- statistical_function(...)

      if (display) {

        cat(
          "\n",
          toupper(requested),
          "\n",
          paste(
            rep("-", nchar(requested)),
            collapse = ""
          ),
          "\n",
          sep = ""
        )

        cat(
          "Function: ",
          technical_name,
          "\n\n",
          sep = ""
        )

        # Preserve any function-specific print methods.
        print(result)
      }

      return(invisible(result))
    }

    # --------------------------------------------------------
    # Unknown commands: show practical help.
    # --------------------------------------------------------

    stop(
      paste0(
        "Unknown analysis: '", task, "'.\n",
        "Run bambagana('methods') for common analyses, ",
        "or bambagana('all functions') to see ",
        "all available commands.\n",
        "Example: bambagana('confidence interval', scores)"
      ),
      call. = FALSE
    )
  }
})

# END BAMBAGANA COMPLETE FRIENDLY INTERFACE


