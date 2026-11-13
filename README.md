# bambaganaCI

**Confidence intervals and classical statistical inference in R.**

Package version: 0.3.0 — 40 statistical functions and one searchable catalog (41 exports)

Authors: Silas Bambil and Yasmeen Babagana.

Both authors contributed equally to the original project.
ChatGPT assisted with implementation, troubleshooting,
testing, and documentation.

## Start here

```r
vignette("getting-started", package = "bambaganaCI")
help(package = "bambaganaCI")
sort(getNamespaceExports("bambaganaCI"))
```

The getting-started guide covers choosing a function, reading intervals, extracting
results, comparing groups, regression, and planning sample size. FUNCTIONS.md groups
all 40 statistical functions by task.

## Expanded toolkit

This development build retains the three original functions and adds 37.
The new interfaces use established R methods where available. They do not
introduce new statistical theory or guarantee coverage across all populations.
See FUNCTIONS.md for the complete 40-function catalogue and each help page
for assumptions, inputs, outputs, and an executable example.

```r
library(bambaganaCI)
set.seed(1998)
describe_numeric(c(8, 10, 12, 9, 15, 11, 13, 10, 14, 12))
proportion_ci(40, 100, method = "wilson")
mean_difference_ci(1:10, 3:14)  # Welch; first mean minus second
fit <- ols_fit(mpg ~ wt + hp, mtcars)
ols_coef_ci(fit)
sample_size_mean(delta = 0.5, sd = 1, power = 0.8)
```

New interval functions return a bambagana_interval data frame with estimate, lower, upper,
conf, method, and assumptions. They print a readable summary; use coef(),
confint(), and as.data.frame() to extract stored values. Proportions print as
percentages but keep their numeric 0-to-1 values. The original mean functions retain their existing
S3 outputs. Test functions return htest objects. Regression and ANOVA use native
R model objects. Power results and extractors have their documented output types.
Missing observations are rejected rather than silently dropped. Rank-test and
sparse-table warnings from R are retained. OLS coefficient and prediction
intervals are classical, not robust to heteroscedasticity or clustered errors.

The order-statistic median interval may be unbounded for very small samples.
The sign-test interval describes positive-sign probability, not the median.
The two-proportion difference interval is R's asymptotic interval, not an exact
interval. These distinctions are deliberate and are documented in help pages.

This remains a development candidate pending a local check on the authors' Mac
and broader method-specific evaluation. Original simulation and timing evidence
below concerns the original mean methods; it is not validation of all additions.

## Original mean-interval methods

`bambaganaCI` provides classical and bootstrap confidence
intervals for a population mean.

Five methods are available:

- Percentile bootstrap
- Basic bootstrap
- Normal bootstrap
- Bias-corrected and accelerated (BCa) bootstrap
- Classical Student's t interval

The package also provides `compare_mean_ci()` for
comparing multiple intervals on the same dataset.

## Installation

Install from a local source archive after downloading it to your computer:

```r
install.packages(
  "bambaganaCI_0.3.0.tar.gz",
  repos = NULL,
  type = "source"
)

library(bambaganaCI)
```

The `boot` package is an imported dependency.
This package is not currently on CRAN. The original v0.1.0 archive is unchanged;
the maintenance updates are being tested separately.

## Basic example

```r
library(bambaganaCI)

set.seed(1998)
x <- rnorm(30, mean = 2, sd = 1)

result <- bootstrap_mean_ci(
  x,
  B = 2000,
  conf = 0.95,
  method = "percentile"
)

print(result)
confint(result)
coef(result)
as.data.frame(result)
```

## Compare confidence interval methods

```r
set.seed(1998)
x <- rexp(30)

comparison <- compare_mean_ci(
  x,
  B = 2000,
  conf = 0.95
)

print(comparison)
```

The comparison includes the interval endpoints,
sample mean, interval width, confidence level,
and sample size.

Select particular methods when needed:

```r
set.seed(1998)

compare_mean_ci(
  x,
  B = 2000,
  methods = c("percentile", "bca", "t")
)
```

Bootstrap methods within a single `compare_mean_ci()` call share resamples.
A separate call to `bootstrap_mean_ci()` may use a different resampling stream
even with the same seed, so do not expect identical endpoints across functions.
The classical t interval does not require resampling.

## Classical t confidence interval

```r
result_t <- mean_t_ci(x, conf = 0.95)
print(result_t)
confint(result_t)
```

## Statistical assumptions

Bootstrap resampling assumes independent and identically
distributed observations. The classical t interval has
exact coverage under independent normal observations
with a common positive variance.

Coverage may differ from its nominal level under
skewness, heavy tails, contamination, and small samples.
No confidence interval method is universally preferable.

The package estimates the ordinary population mean.
It does not automatically handle clustered observations,
time-series dependence, survey weights, or missing data.

## Numerical reliability

The package validates numeric inputs and provides
numerical scaling safeguards for extreme values.

Constant samples produce zero-width empirical bootstrap
intervals. Such intervals do not establish zero
population uncertainty. The classical t interval
rejects constant samples.

BCa endpoints can be sensitive to tied bootstrap
statistics, floating-point rounding, and poorly resolved bootstrap tails.
BCa is not guaranteed to have better coverage than simpler intervals,
particularly with small samples, heavy tails, or contamination.

Set the random seed before calling bootstrap functions
when reproducibility is required.

## Testing and validation

The package includes automated unit and regression
tests for interval construction, input validation,
reproducibility, numerical scaling, and S3 methods.

A separate Monte Carlo validation study evaluated
confidence interval coverage and width under normal,
exponential, heavy-tailed, and contaminated sampling.

The separate validation study used 10,000 Monte Carlo replications per
setting and 2,000 bootstrap resamples. It is not bundled as part of
the package installation.

Validation results describe performance under the
evaluated simulation conditions; they do not guarantee
nominal coverage for arbitrary populations.

## License

MIT.

## Software acknowledgement

ChatGPT was used to assist with code development,
debugging, testing, and documentation. The authors
retain responsibility for the software and its
statistical claims.


<!-- BEGIN BAMBAGANA SIMPLE INPUT GUIDE -->

## Getting started: simple statistical analyses

The `bambagana()` function provides a short interface for common
statistical procedures. It uses the same underlying calculations
as the individual functions in the package.

```r
library(bambaganaCI)

group_A <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)
group_B <- c(71, 79, 75, 82, 77, 85, 80, 74, 78, 81)

# Confidence interval for one mean
bambagana("mean", group_A)

# Confidence interval for the difference in means
bambagana("compare means", group_A, group_B)

# One-sample hypothesis test
bambagana("test mean", group_A, mu = 70)

# Welch two-sample hypothesis test
bambagana("test two means", group_A, group_B)

# A proportion: 65 successes in 100 trials
bambagana("proportion", c(65, 100))

# Difference between independent proportions
bambagana("compare proportions", c(65, 100), c(45, 100))
```

For paired samples, observations must correspond in order:

```r
before <- c(130, 125, 142, 138, 150, 145, 135, 148)
after  <- c(125, 120, 135, 132, 141, 138, 130, 140)

bambagana("paired", before, after)
```

Bootstrap intervals are also available:

```r
set.seed(1998)
bambagana("bootstrap", group_A, B = 999, method = "bca")
```

Regression and ANOVA use their familiar R inputs:

```r
df <- data.frame(
  experience = 1:10,
  salary = c(32, 35, 37, 41, 43, 46, 50, 51, 55, 59)
)

bambagana("regression", salary ~ experience, data = df)

scores <- c(65, 72, 68, 75, 70, 78, 74, 69, 71, 79, 75, 82)
groups <- factor(rep(c("A", "B", "C"), each = 4))

bambagana("anova", scores, groups)
```

### Results and advanced use

The interface displays a formatted summary automatically.
The original statistical result is still returned.

```r
result <- bambagana("mean", group_A)

# Access the numerical result
result$estimate
result$lower
result$upper

# Suppress automatic output
result <- bambagana("mean", group_A, display = FALSE)

# Display a result explicitly
bambagana_show(result)
```

For traditional R objects, the saved result retains its usual class.
For example, a one-sample t-test still returns an `htest` object.

Use `bambagana("methods")` for the list of supported analyses
and example calls.

The existing individual functions, including `mean_t_ci()`,
`proportion_difference_ci()`, and `ols_fit()`, remain available
for users who prefer direct access.


### Readable descriptive statistics

```r
bambagana("describe", group_A)
```

This displays observations, mean, median, standard deviation, variance, minimum, maximum, and quartiles. The original data frame is retained in the returned result.

Proportion intervals retain success counts and percentage units. Two-sample tests also explain the observed direction of the sample-mean difference.

<!-- END BAMBAGANA SIMPLE INPUT GUIDE -->



## Using all statistical functions

The `bambagana()` interface provides readable commands for all 40 statistical functions.

```r
bambagana("confidence interval", scores)
bambagana("descriptive statistics", scores)
bambagana("standard error", scores)
bambagana("compare averages", group_A, group_B)
bambagana("hypothesis test", scores, mu = 70)
bambagana("bootstrap confidence interval", scores, B = 999)
bambagana("all functions")
```

The full function list shows the readable command, underlying R function, and formal arguments. The original statistical functions remain accessible directly. All existing `bambagana()` commands are preserved.

