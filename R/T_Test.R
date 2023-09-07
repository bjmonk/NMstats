#' Hypothesis Test for a Population Mean, Standard Deviation Unknown
#'
#' This function performs a hypothesis test about a population mean when the population standard deviation is not known. P-values are calculated based on the Student's t-distribution.
#'
#' @param xbar   The sample mean.
#' @param n      The sample size.
#' @param s      The sample standard deviation.
#' @param mu     The hypothesized mean in the null hypothesis.
#' @param alt    A character string specifying the alternate hypothesis. Choices are "left", "right", or "two". If omitted, the default is "two". The choices "less", "greater", or "two.sided" are also accepted.
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
#'
#' @export


T_Test <- function(xbar, n, s, mu, alt = "two") {

  # Check that n is a positive integer
  if (floor(n) != n || n <= 0) {
    stop("Error: Sample size 'n' must be a positive integer.")
  }

  # Check that s is a positive number
  if (s <= 0) {
    stop("Error: Sample standard deviation 's' must be a positive number.")
  }

  # Check that xbar and mu are real numbers
  if (is.na(xbar) || is.infinite(xbar) || is.na(mu) || is.infinite(mu)) {
    stop("Error: Sample mean 'xbar' and hypothesized mean 'mu' must be real numbers.")
  }

  # Check that alternate is one of the allowed values
  alt <- match.arg(alt, choices = c("left", "right", "two", "less", "greater", "two.sided"))

  # Map alternate names
  if (alt == "less") alt <- "left"
  if (alt == "greater") alt <- "right"
  if (alt == "two.sided") alt <- "two"

  # Calculate the test statistic
  t <- (xbar - mu) / (s / sqrt(n))

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
    "Number of Degrees of Freedom:", round(df, 5),
    "\nTest Statistic: t =", round(t, 5),
    "\nP-Value:", round(p_val, 5),
    "\n"
  )

  # Return the values as a named list for further use
  result <- list(t = t, pvalue = p_val)

  return(invisible(result))
}
