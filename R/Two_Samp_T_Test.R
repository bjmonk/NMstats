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
#' @param mu0     The hypothesized mean in the null hypothesis. If omitted, `mu0` is assumed to be 0.
#' @details The function will print the number of degrees of freedom, the test statistic t and the p-value for the test about `mu_1 - mu_2`.
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
#' @export

Two_Samp_T_Test <-function(xbar1, s1, n1, xbar2, s2, n2, alt = "two", mu0 = 0)
{

  # Check that alternate is one of the allowed values
  alt <- match.arg(alt, choices = c("left", "right", "two", "less", "greater", "two.sided"))

  # Map alternate names
  if (alt == "less") alt <- "left"
  if (alt == "greater") alt <- "right"
  if (alt == "two.sided") alt <- "two"

  # Calculate the test statistic
  t <- (xbar1 - xbar2 - mu0) / sqrt( s1^2 / n1 + s2^2 / n2)

  # Calculate the degrees of freedom
  df <- ((s1^2 / n1 + s2^2 / n2)^2) / ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))

  # Calculate p-values
  if(alt == "left") { p_val <- pt(t, df) }
  if(alt == "right") { p_val <- 1 - pt(t, df) }
  if(alt == "two") { p_val <- 2 * (1 - pt(abs(t), df)) }

  # Print the results in a user-friendly manner
  cat(
    "Number of Degrees of Freedom:",
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
