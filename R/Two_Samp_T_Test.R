#' Hypothesis Test for the Difference Between Two Means: Independent Samples
#'
#' This function performs a hypothesis test about the difference between two population means (`mu_1 - mu_2`) given two independent samples. P-values are calculated based on the Student's t-distribution.
#'
#' @param xbar1   The mean of Sample 1.
#' @param s1      The standard deviation of Sample 1.
#' @param n1      The size of Sample 1.
#' @param xbar2   The mean of Sample 2.
#' @param s2      The standard deviation of Sample 2.
#' @param n2      The size of Sample 2.
#' @param alt     A required character string specifying the alternate hypothesis. Choices are "left", "right", or "two". The choices "less", "greater", or "two.sided" are also accepted.
#' @param mu     The hypothesized mean in the null hypothesis. If omitted, `mu` is assumed to be 0.
#' @details The function will print the number of degrees of freedom, the test statistic t and the p-value for the test about `mu_1 - mu_2`.
#'
#' @return  The function returns, invisibly, a list containing:
#' \itemize{
#'       \item \code{t}: The test statistic t.
#'       \item \code{pvalue}: The p-value corresponding to the test statistic.
#'     }
#' You can access these values with expressions like \code{res$t}, and \code{res$pvalue} where \code{res} is the result of the function.
#'
#' If invalid input values are given, an error message is returned.
#'
#'@importFrom stats pt
#'
#' @export

Two_Samp_T_Test <- function(xbar1, s1, n1, xbar2, s2, n2, alt = "two", mu = 0) {

  # Check that sample sizes n1 and n2 are positive integers
  if (floor(n1) != n1 || n1 <= 0 || floor(n2) != n2 || n2 <= 0) {
    stop("Error: Sample sizes 'n1' and 'n2' must be positive integers.")
  }

  # Check that standard deviations s1 and s2 are positive numbers
  if (s1 <= 0 || s2 <= 0) {
    stop("Error: Standard deviations 's1' and 's2' must be positive numbers.")
  }

  # Validate alternative hypothesis
  alt <- match.arg(alt, choices = c("left", "right", "two", "less", "greater", "two.sided"))

  # Map alternate names
  if (alt == "less") alt <- "left"
  if (alt == "greater") alt <- "right"
  if (alt == "two.sided") alt <- "two"

  # Calculate test statistic and degrees of freedom
  t <- (xbar1 - xbar2 - mu) / sqrt(s1^2 / n1 + s2^2 / n2)
  df <- ((s1^2 / n1 + s2^2 / n2)^2) / ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))

  # Calculate p-value based on alternative hypothesis
  if (alt == "left") { p_val <- pt(t, df) }
  if (alt == "right") { p_val <- 1 - pt(t, df) }
  if (alt == "two") { p_val <- 2 * (1 - pt(abs(t), df)) }

  # Print the results
  cat(
    "Number of Degrees of Freedom:",
    round(df, 5),
    "\nTest Statistic: t =",
    round(t, 5),
    "\nP-Value:",
    round(p_val, 5),
    "\n"
  )

  # Return t and p-value
  result <- list(t = t, pvalue = p_val)

  return(invisible(result))
}
