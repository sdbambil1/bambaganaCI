## 0.3.3

- Extended the beginner-facing `bambagana()` interface to every exported statistical function.
- Added readable aliases and a complete function inventory with formal argument names.
- Preserved the 40 statistical implementations, 43 exports, and existing commands.

# bambaganaCI development notes

## 0.3.0

- Expanded the package to 40 statistical functions.
- Added `bambagana_catalog()` for searching methods by task.
- Improved the getting-started vignette and method guidance.
- Expanded documentation of bootstrap assumptions,
  numerical reliability, and empirical coverage limitations.
- Added permanent BCa regression tests against
  `boot::boot()` and `boot::boot.ci()`.
- Verified BCa reference agreement for normal,
  exponential, and Student t(3) samples.
- Documented the 4,000-dataset bootstrap coverage study.
- Preserved the existing statistical implementations,
  bootstrap defaults, and established method interfaces.
- Completed a full package build and check during
  development with no errors, warnings, or notes.


## 0.1.1

- Expanded validation and constant-data printing tests.
- Added safeguards in printing and interval comparisons.
- Clarified resampling and numerical limitations in the README.
- Removed obsolete development test scripts from the maintained source.

## 0.1.0

- Initial `bambaganaCI` release, providing percentile, basic, normal, BCa,
  and Student's t confidence intervals for a population mean.


<!-- BEGIN BAMBAGANA SIMPLE INPUT NEWS -->

## 0.3.2

- Added readable descriptive statistics through `bambagana("describe", x)`.
- Standardized the beginner-facing proportion and proportion-difference displays while preserving units.
- Added an interpretation of the observed difference and inferential evidence for two-sample mean tests.
- Preserved the numerical methods, return objects, and existing public API.

## 0.3.1

- Improved `bambagana()` with contextual input validation and
  examples in error messages.
- Added automatic console presentation for common analyses.
- Added `display = FALSE` for programmatic use without printing.
- Preserved original statistical result objects and calculations.
- Documented the simple interface in the README and vignette.
- Added regression tests for input guidance, display behavior,
  and numerical equivalence.


<!-- END BAMBAGANA SIMPLE INPUT NEWS -->


