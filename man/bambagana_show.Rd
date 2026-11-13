
\name{bambagana_show}
\alias{bambagana_show}
\title{Display a bambaganaCI Statistical Result}
\description{
Provides a consistent console display of statistical results
without modifying the underlying object.
}
\usage{
bambagana_show(result, details = FALSE, digits = 4L, title = NULL)
}
\arguments{
\item{result}{A statistical result object.}
\item{details}{Logical. Show the original technical output.}
\item{digits}{Number of significant digits, from 1 to 15.}
\item{title}{Optional character heading.}
}
\value{
The original result, returned invisibly.
}
\details{
Supports confidence intervals, hypothesis tests, linear models,
analysis of variance, numeric estimates, and the method catalog.

Standard R print methods are not overwritten. Statistical calculations
and original result structures are unchanged.

The display conclusion for hypothesis tests uses a fixed 5 percent
significance level. This is a presentation convention, not an
additional statistical calculation.
}
\examples{
x <- c(65, 72, 68, 75, 70, 78, 74, 69, 73, 76)
result <- mean_t_ci(x)
bambagana_show(result)
bambagana_show(result, details = TRUE)
}

