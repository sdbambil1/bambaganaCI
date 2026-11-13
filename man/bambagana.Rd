
% bambaganaCI simple input interface v1
\name{bambagana}
\alias{bambagana}
\title{Simple Statistical Analysis Interface}
\description{
Runs common statistical procedures using readable analysis names.
All calculations are delegated to the existing bambaganaCI functions.
}
\usage{
bambagana(task, ..., display = TRUE)
}
\arguments{
\item{task}{Character string identifying the analysis.}


\item{...}{Analysis-specific arguments passed to the selected statistical function. For example, \code{x} supplies the first data vector, \code{y} supplies a second vector where needed, and \code{conf} specifies the confidence level (default 0.95 for supported interval methods). Other arguments depend on the selected analysis. Use \code{bambagana("all functions")} to inspect the available functions and arguments.}

\item{display}{If \code{TRUE}, display the formatted result.
If \code{FALSE}, suppress the interface display.}
}
\details{
The beginner-facing interface also recognizes readable analysis names such as \code{"confidence interval"}, \code{"standard error"}, and \code{"linear regression"}. Use \code{bambagana("all functions")} to list every available statistical function and its arguments. Existing commands and numerical calculations are preserved.

The analysis name \code{"describe"} provides a readable
summary of sample size, location, and dispersion.
Proportion displays retain successes, trials, and percentage units.
Two-sample mean tests additionally explain the observed
sample-mean difference without making causal claims.

Use \code{bambagana("methods")} or \code{bambagana("help")}
to display available analyses and examples.

For proportions, \code{c(65, 100)} represents 65 successes
in 100 trials. For paired observations, supply two vectors
in corresponding order.

The function does not change the statistical methods or
their returned objects. Default printing is designed for
interactive console use. Setting \code{display = FALSE}
is useful in scripts and for retrieving results without
console output.
}
\value{
For statistical analyses, the original result from the
selected function, returned invisibly when \code{display = TRUE}
and visibly when \code{display = FALSE}.
For \code{"methods"} and \code{"help"}, an invisible
character vector of supported analyses.
}
\examples{
x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)
y <- c(71, 79, 75, 82, 77, 85, 80, 74, 78, 81)

bambagana("describe", x)
bambagana("mean", x)
bambagana("compare means", x, y)
bambagana("proportion", c(65, 100))
bambagana("compare proportions", c(65, 100), c(45, 100))
bambagana("test mean", x, mu = 70)

result <- bambagana("mean", x, display = FALSE)
result$estimate
}

