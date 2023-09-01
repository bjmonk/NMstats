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
#'@importFrom stats qt
#'

Two_Samp_T_Interval <- function(xbar1, s1, n1, xbar2, s2, n2, alpha)
{
  # Calculate confidence level
  conf_lev <- (1 - alpha) * 100

  # Calculate the degrees of freedom
  df <-
    ((s1 ^ 2 / n1 + s2 ^ 2 / n2) ^ 2) / ((s1 ^ 2 / n1) ^ 2 / (n1 - 1) + (s2 ^
                                                                           2 / n2) ^ 2 / (n2 - 1))
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
    "\nConfidence Level:",
    round(conf_lev, 4), "%",
    "\nMargin of Error:",
    round(margin_error, 4),
    "\nNumber of Degrees of Freedom:",
    round(df, 4),
    "\nCritical Value:",
    round(crit_val, 4),
    "\nPoint Estimate:",
    round(point_est, 4),
    "\nLower Bound:",
    round(lower, 4),
    "\nUpper Bound:",
    round(upper, 4),
    "\n"
  )

  # Return the critical value, lower bound, and upper bound values as a named list for further use
  result <- list(critval = crit_val,
                 point_est = point_est,
                 Lbound = lower,
                 Ubound = upper)

  return(invisible(result))
}
