
\name{bambagana_catalog}
\alias{bambagana_catalog}
\title{Explore Available Statistical Methods}
\description{
Search statistical methods provided by bambaganaCI.
}
\usage{
bambagana_catalog(search = NULL)
}
\arguments{
\item{search}{
NULL or a nonempty character string used to search
function names, categories, and descriptions.
}
}
\value{
A data frame containing function names, categories,
and descriptions.
}
\details{
The catalog helps users discover available functions.
It does not establish whether a particular statistical
method is appropriate for a study.
}
\examples{
bambagana_catalog()
bambagana_catalog("confidence")
bambagana_catalog("regression")
bambagana_catalog("proportion")
}

