# bambaganaCI

**Confidence intervals and classical statistical inference in R**

**Version 0.3.4** · 40 statistical functions · 43 public exports

**Authors:** Silas Bambil and Yasmeen Babagana. Both authors contributed equally to the original project.

bambaganaCI provides a beginner-friendly `bambagana()` interface for descriptive statistics, confidence intervals, bootstrap methods, hypothesis testing, association measures, linear regression, analysis of variance, and power planning. The original statistical functions remain directly accessible. The package implements or interfaces with established methods rather than proposing new statistical theory.

## Installation

Install the released version from GitHub:

```r
install.packages("remotes") # if needed
remotes::install_github("sdbambil1/bambaganaCI", ref = "v0.3.4")
library(bambaganaCI)
```

This GitHub release is separate from CRAN publication. **The package has not been accepted by or submitted to CRAN.**

## Examples

```r
library(bambaganaCI)
scores <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)
other  <- c(71, 79, 75, 82, 77, 85, 80, 74, 78, 81)
bambagana("descriptive statistics", scores)
bambagana("confidence interval", scores)
bambagana("compare averages", scores, other)
bambagana("hypothesis test", scores, mu = 70)
bambagana("all functions")
set.seed(1998)
bambagana("bootstrap confidence interval", scores, B = 999)
```

For additional examples and method-specific assumptions, see the [package documentation](https://sdbambil1.github.io/bambaganaci.html), the installed help pages, and the getting-started vignette.

## Methods and limitations

Bootstrap inference for the population mean ordinarily assumes independent, identically distributed observations. Classical t intervals are exact under independent normal sampling with a common positive variance. Coverage and test behavior can differ under skewness, heavy tails, sparse observations, clustering, and other departures from the assumptions. This package does not automatically account for complex survey designs or serial dependence.

## Validation

The exact v0.3.4 source archive passed `R CMD check --as-cran` with no errors or warnings on macOS R 4.4.1, Windows R-devel (Winbuilder), and Ubuntu Linux R 4.6.2. The Linux run reported one expected `New submission` NOTE. Local macOS checks reported an additional system-time verification NOTE. The Windows R-release result has not been independently verified. Technical checks do not guarantee coverage under every data-generating process.

Linux validation: https://github.com/sdbambil1/bambaganaCI/actions/runs/43519880509

## License and acknowledgements

MIT License. ChatGPT assisted with code development, troubleshooting, testing, and documentation. The authors remain responsible for the software and its statistical claims.
