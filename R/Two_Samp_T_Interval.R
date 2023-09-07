#' Confidence Interval for the Difference Between Two Means: Independent Samples
#'
#' This function constructs a confidence interval for the difference between two population means (`mu_1 - mu_2`) given two independent samples. This confidence interval is based on the Student's t-distribution.
#'
#' @param xbar1   The mean of Sample 1.
#' @param s1      The standard deviation of Sample 1.
#' @param n1      The size of Sample 1.
#' @param xbar2   The mean of Sample 2.
#' @param s2      The standard deviation of Sample 2.
#' @param n2      The size of Sample 2.
#' @param alpha   A value between 0 and 1, where `1 - alpha` is the confidence level as a decimal. The default is 0.05, representing a 95% confidence level.
#'
#' @details The function will print the confidence level, margin of error, number of degrees of freedom, critical value, point estimate, and the confidence interval (lower and upper bounds).
#'
#' @return  The function returns, invisibly, a list containing:
#' \itemize{
#'       \item \code{critval}: The critical value used in calculations.
#'       \item \code{point_est}: The point estimate for the confidence interval.
#'       \item \code{Lbound}: The lower boundary of the confidence interval.
#'       \item \code{Ubound}: The upper boundary of the confidence interval.
#'     }
#' You can access these values with expressions like \code{res$critval}, \code{res$Lbound}, and \code{res$Ubound} where \code{res} is the result of the function.
#'
#' If invalid input values are given, an error message is returned.
#'
#' @importFrom stats qt
#'
#' @export

Two_Samp_T_Interval <- function(xbar1, s1, n1, xbar2, s2, n2, alpha = 0.05) {

  # Check that alpha is between 0 and 1
  if (alpha <= 0 || alpha >= 1) {
    stop("Error: Alpha must be between 0 and 1.")
  }

  # Check that sample sizes n1 and n2 are positive integers
  if (floor(n1) != n1 || n1 <= 0 || floor(n2) != n2 || n2 <= 0) {
    stop("Error: Sample sizes 'n1' and 'n2' must be positive integers.")
  }

  # Check that standard deviations s1 and s2 are positive numbers
  if (s1 <= 0 || s2 <= 0) {
    stop("Error: Standard deviations 's1' and 's2' must be positive numbers.")
  }

  # Calculate confidence level
  conf_lev <- (1 - alpha) * 100

  # Calculate the degrees of freedom
  df <- ((s1 ^ 2 / n1 + s2 ^ 2 / n2) ^ 2) /
    ((s1 ^ 2 / n1) ^ 2 / (n1 - 1) + (s2 ^ 2 / n2) ^ 2 / (n2 - 1))

  # Calculate critical value
  crit_val <- qt(1 - alpha / 2, df)

  # Calculate margin of error
  margin_error <- crit_val * sqrt(s1 ^ 2 / n1 + s2 ^ 2 / n2)

  # Calculate the point estimate
  point_est <- xbar1 - xbar2

  # Calculate lower and upper bounds of confidence interval
  lower <- point_est - margin_error
  upper <- point_est + margin_error

  # Print the results in a user-friendly manner
  cat(
    "Confidence Level:", round(conf_lev, 5), "%",
    "\nMargin of Error:", round(margin_error, 5),
    "\nNumber of Degrees of Freedom:", round(df, 5),
    "\nCritical Value:", round(crit_val, 5),
    "\nPoint Estimate:", round(point_est, 5),
    "\nLower Bound:", round(lower, 5),
    "\nUpper Bound:", round(upper, 5),
    "\n"
  )

  # Return the critical value, point estimate, and confidence interval bounds
  result <- list(
    critval = crit_val,
    point_est = point_est,
    Lbound = lower,
    Ubound = upper
  )

  return(invisible(result))
}
