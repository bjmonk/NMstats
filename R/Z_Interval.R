#' Z Confidence Interval for a Population Mean
#'
#' This function calculates the confidence interval for a population mean when the population standard deviation is known. This confidence interval is based on the normal distribution, which is appropriate when the population standard deviation is known.
#'
#' @param xbar   The sample mean.
#' @param n      The sample size.
#' @param sigma  The known population standard deviation.
#' @param alpha  A value between 0 and 1, where `1 - alpha` is the confidence level as a decimal. The default is 0.05, representing a 95% confidence level.
#'
#' @details The function will print the confidence level, margin of error, critical value, and the confidence interval (lower and upper bounds).
#'
#' @return  The function returns, invisibly, a list containing:
#' \itemize{
#'       \item \code{critval}: The critical value used in calculations.
#'       \item \code{Lbound}: The lower boundary of the confidence interval.
#'       \item \code{Ubound}: The upper boundary of the confidence interval.
#'     }
#' You can access these values with expressions like \code{res$critval}, \code{res$Lbound}, and \code{res$Ubound} where \code{res} is the result of the function.
#'
#' @importFrom stats qnorm
#'
#' @note This function assumes the population standard deviation is known. If the population standard deviation is not known, consider using the \code{\link{T_Interval}} function, based on the Student's t-distribution.


Z_Interval <- function(xbar, n, sigma, alpha = 0.05) {
  #... [Function implementation]
}



Z_Interval <- function(xbar, n, sigma, alpha = 0.05)
{
  # Calculate confidence level
  conf_lev <- (1 - alpha) * 100

  # Calculate critical value
  crit_val <- qnorm(1 - alpha / 2)

  # Calculate margin of error
  margin_error <- crit_val * sigma / sqrt(n)

  # Calculate lower and upper bounds of confidence interval
  lower <- xbar - margin_error
  upper <- xbar + margin_error

  # Print the results in a user-friendly manner
  cat(
    "\nConfidence Level:",
    round(conf_lev, 4), "%",
    "\nMargin of Error:",
    round(margin_error, 4),
    "\nCritical Value:",
    round(crit_val, 4),
    "\nLower Bound:",
    round(lower, 4),
    "\nUpper Bound:",
    round(upper, 4),
    "\n"
  )

  # Return the critical value, lower bound, and upper bound values as a named list for further use
  result <- list(critval = crit_val,
                 Lbound = lower,
                 Ubound = upper)

  return(invisible(result))
}
