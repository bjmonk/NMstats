#' Confidence Interval for the Difference Between Two Proportions
#'
#' This function calculates the confidence interval for the difference between two population proportions (`p_1 - p_2`).
#'
#' @param x1   The number of individuals of interest in Sample 1.
#' @param n1   The size of Sample 1.
#' @param x2   The number of individuals of interest in Sample 2.
#' @param n2   The size of Sample 2.
#' @param alpha  A value between 0 and 1, where `1 - alpha` is the confidence level as a decimal. The default is 0.05, representing a 95% confidence level.
#'
#' @details The function will print the confidence level, margin of error, critical value, point estimate, and the confidence interval (lower and upper bounds).
#'
#' @return  The function returns, invisibly, a list containing:
#' \itemize{
#'       \item \code{sprop1}: Sample 1 proportion.
#'       \item \code{sprop2}: Sample 2 proportion.
#'       \item \code{point_est}: The point estimate for the confidence interval.
#'       \item \code{critval}: The critical value used in calculations.
#'       \item \code{Lbound}: The lower boundary of the confidence interval.
#'       \item \code{Ubound}: The upper boundary of the confidence interval.
#'     }
#' You can access these values with expressions like \code{res$sprop1}, \code{res$point_est}, and \code{res$Ubound} where \code{res} is the result of the function.
#'
#' @importFrom stats qnorm
#'
#' @export


Two_Prop_Int <- function(x1, n1, x2, n2, alpha = 0.05)
{
  # Input validation
  if (!is.numeric(x1) ||
      !is.numeric(x2) || !is.numeric(n1) || !is.numeric(n2)) {
    stop("All inputs x1, n1, x2, n2 must be numeric.")
  }

  if (x1 < 0 || x2 < 0) {
    stop("The number of successes (x1 and x2) must be non-negative.")
  }

  if (n1 <= 0 || n2 <= 0) {
    stop("Sample sizes (n1 and n2) must be greater than zero.")
  }

  if (x1 > n1) {
    stop("The number of successes (x1) cannot be greater than the sample size (n1).")
  }

  if (x2 > n2) {
    stop("The number of successes (x2) cannot be greater than the sample size (n2).")
  }

  if (alpha <= 0 || alpha >= 1) {
    stop("The significance level (alpha) must be between 0 and 1.")
  }


  # Calculate confidence level
  conf_lev <- (1 - alpha) * 100

  # Calculate sample proportions
  phat1 <- x1 / n1
  phat2 <- x2 / n2

  # Calculate critical value
  crit_val <- qnorm(1 - alpha / 2)

  # Calculate point estimate
  point_est <- phat1 - phat2

  # Calculate margin of error
  margin_error <-
    crit_val * sqrt(phat1 * (1 - phat1) / n1 + phat2 * (1 - phat2) / n2)

  # Calculate lower and upper bounds of confidence interval
  lower <- point_est - margin_error
  upper <- point_est + margin_error

  # Print the results in a user-friendly manner
  cat(
    "Confidence Level:",
    round(conf_lev, 5),
    "%",
    "\nMargin of Error:",
    round(margin_error, 5),
    "\nCritical Value:",
    round(crit_val, 5),
    "\nPoint Estimate:",
    round(point_est, 5),
    "\nLower Bound:",
    round(lower, 5),
    "\nUpper Bound:",
    round(upper, 5),
    "\n"
  )

  # Return the sample proportion, critical value, lower bound, and upper bound values as a named list for further use
  result <- list(
    sprop1 = phat1,
    sprop2 = phat2,
    point_est = point_est,
    critval = crit_val,
    Lbound = lower,
    Ubound = upper
  )

  return(invisible(result))
}
