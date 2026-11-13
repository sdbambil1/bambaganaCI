# bambaganaCI

**Confidence intervals for a population mean in R.**

Current package version: 0.1.1

Authors: Silas Bambil and Yasmeen Babagana.

Both authors contributed equally to the original project.
ChatGPT assisted with implementation, troubleshooting,
testing, and documentation.

## Overview

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

Install the current package source from the public GitHub repository:

```r
install.packages("remotes")  # If remotes is not already installed
remotes::install_github("sdbambil1/bambaganaCI")

library(bambaganaCI)
```

Alternatively, if you already have a source archive on your computer:

```r
install.packages(
  "bambaganaCI_0.1.1.tar.gz",
  repos = NULL,
  type = "source"
)
```

The `boot` package is an imported dependency. This package is not currently
on CRAN. The GitHub installation command uses the current repository source;
it does not imply a CRAN release or a separately verified installation.

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
