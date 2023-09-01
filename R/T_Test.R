#' Hypothesis Test for a Population Mean, Standard Deviation Unknown
#'
#' This function performs a hypothesis test about a population mean when the population standard deviation is not known. P-values are calculated based on the Student's t-distribution.
#'
#' @param xbar   The sample mean.
#' @param n      The sample size.
#' @param s      The sample standard deviation.
#' @param mu0  The hypothesized mean in the null hypothesis.
#' @param alt  A character string specifying the alternate hypothesis. Choices are "left", "right", or "two". If omitted, the default is "two". The choices "less", "greater", or "two.sided" are also accepted.
#'
#' @details The function will print the number of degrees of freedom, the test statistic t and the p-value.
#'
#' @return  The function returns, invisibly, a list containing:
#' \itemize{
#'       \item \code{t}: The test statistic t.
#'       \item \code{pvalue}: The p-value corresponding to the test statistic.
#'     }
#' You can access these values with expressions like \code{res$t}, and \code{res$pvalue} where \code{res} is the result of the function.
#'
#'@importFrom stats pt
#'
#' @note This function is appropriate when the population standard deviation is not known. If the population standard deviation is known, consider using the \code{\link{Z_Test}} function, based on the normal distribution.


T_Test <- function(xbar,
                   n,
                   s,
                   mu0,
                   alt = "two")
{

  # Check that alternate is one of the allowed values
  alt <- match.arg(alt, choices = c("left", "right", "two", "less", "greater", "two.sided"))

  # Map alternate names
  if (alt == "less") alt <- "left"
  if (alt == "greater") alt <- "right"
  if (alt == "two.sided") alt <- "two"

  # Calculate the test statistic
  t <- (xbar - mu0) / (s / sqrt(n))

  # Calculate the number of degrees of freedom
  df <- n - 1

  # Calculate p-values
  if (alt == "left") {
    p_val <- pt(t, df)
  }
  if (alt == "right") {
    p_val <- 1 - pt(t, df)
  }
  if (alt == "two") {
    p_val <- 2 * (1 - pt(abs(t), df))
  }

  # Print the results in a user-friendly manner
  cat(
    "\nNumber of Degrees of Freedom:",
    round(df, 4),
    "\nTest Statistic: t =",
    round(t, 4),
    "\nP-Value:",
    round(p_val, 4),
    "\n"
  )

  # Return the values as a named list for further use
  result <- list(t = t,
                 pvalue = p_val)

  return(invisible(result))
}
