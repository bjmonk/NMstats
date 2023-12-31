% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Z_Test.R
\name{Z_Test}
\alias{Z_Test}
\title{Hypothesis Test for a Population Mean, Standard Deviation Known}
\usage{
Z_Test(xbar, n, sigma, mu, alt = "two")
}
\arguments{
\item{xbar}{The sample mean.}

\item{n}{The sample size.}

\item{sigma}{The population standard deviation.}

\item{mu}{The hypothesized mean in the null hypothesis.}

\item{alt}{A character string specifying the alternate hypothesis. Choices are "left", "right", or "two". If omitted, the default is "two". The choices "less", "greater", or "two.sided" are also accepted.}
}
\value{
The function returns, invisibly, a list containing:
\itemize{
\item \code{z}: The test statistic z.
\item \code{pvalue}: The p-value corresponding to the test statistic.
}
You can access these values with expressions like \code{res$z}, and \code{res$pvalue} where \code{res} is the result of the function.

If invalid input values are given, an error message is returned.
}
\description{
This function performs a hypothesis test about a population mean when the population standard deviation is known. P-values are calculated based on the normal distribution.
}
\details{
The function will print the test statistic z and the p-value.
}
\note{
This function is appropriate when the population standard deviation is known. If the population standard deviation is not known, consider using the \code{\link{T_Test}} function, based on the Student's t-distribution.
}
