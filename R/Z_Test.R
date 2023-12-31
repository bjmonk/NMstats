#' Hypothesis Test for a Population Mean, Standard Deviation Known
#'
#' This function performs a hypothesis test about a population mean when the population standard deviation is known. P-values are calculated based on the normal distribution.
#'
#' @param xbar   The sample mean.
#' @param n      The sample size.
#' @param sigma  The population standard deviation.
#' @param mu  The hypothesized mean in the null hypothesis.
#' @param alt  A character string specifying the alternate hypothesis. Choices are "left", "right", or "two". If omitted, the default is "two". The choices "less", "greater", or "two.sided" are also accepted.
#'
#' @details The function will print the test statistic z and the p-value.
#'
#' @return  The function returns, invisibly, a list containing:
#' \itemize{
#'       \item \code{z}: The test statistic z.
#'       \item \code{pvalue}: The p-value corresponding to the test statistic.
#'     }
#' You can access these values with expressions like \code{res$z}, and \code{res$pvalue} where \code{res} is the result of the function.
#'
#'If invalid input values are given, an error message is returned.
#'
#'@importFrom stats pnorm
#'
#' @note This function is appropriate when the population standard deviation is known. If the population standard deviation is not known, consider using the \code{\link{T_Test}} function, based on the Student's t-distribution.
#'
#' @export

Z_Test <- function(xbar, n, sigma, mu, alt = "two") {
  # Check that n is a positive integer
  if (floor(n) != n || n <= 0) {
    stop("Error: Sample size 'n' must be a positive integer.")
  }

  # Check that sigma is a positive number
  if (sigma <= 0) {
    stop("Error: Standard deviation 'sigma' must be a positive number.")
  }

  # Check that xbar and mu are real numbers
  if (is.na(xbar) ||
      is.infinite(xbar) || is.na(mu) || is.infinite(mu)) {
    stop("Error: Sample mean 'xbar' and hypothesized mean 'mu' must be real numbers.")
  }

  # Check that alternate is one of the allowed values
  alt <-
    match.arg(alt,
              choices = c("left", "right", "two", "less", "greater", "two.sided"))

  # Map alternate names
  if (alt == "less")
    alt <- "left"
  if (alt == "greater")
    alt <- "right"
  if (alt == "two.sided")
    alt <- "two"

  # Calculate the test statistic
  z <- (xbar - mu) / (sigma / sqrt(n))

  # Calculate p-values
  if (alt == "left") {
    p_val <- pnorm(z)
  }
  if (alt == "right") {
    p_val <- 1 - pnorm(z)
  }
  if (alt == "two") {
    p_val <- 2 * (1 - pnorm(abs(z)))
  }

  # Print the results in a user-friendly manner
  cat("Test Statistic: z =",
      round(z, 5),
      "\nP-Value:",
      round(p_val, 5),
      "\n")

  # Return the values as a named list for further use
  result <- list(z = z, pvalue = p_val)

  return(invisible(result))
}
