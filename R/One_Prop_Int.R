#' One Sample Confidence Interval for a Population Proportion
#'
#' This function calculates the confidence interval for a population proportion using the normal distribution.
#'
#' @param x   The number of individuals of interest in the sample.
#' @param n   The sample size.
#' @param alpha  A value between 0 and 1, where `1 - alpha` is the confidence level as a decimal. The default is 0.05, representing a 95% confidence level.
#'
#' @details The function will print the confidence level, margin of error, critical value, sample proportion, and the confidence interval (lower and upper bounds).
#'
#' @return  The function returns, invisibly, a list containing:
#' \itemize{
#'       \item \code{sprop}: The sample proportion.
#'       \item \code{critval}: The critical value used in calculations.
#'       \item \code{Lbound}: The lower boundary of the confidence interval.
#'       \item \code{Ubound}: The upper boundary of the confidence interval.
#'     }
#' You can access these values with expressions like \code{res$sprop}, \code{res$critval}, \code{res$Lbound}, and \code{res$Ubound} where \code{res} is the result of the function.
#'
#' @importFrom stats qnorm
#' @export


One_Prop_Int <- function(x, n, alpha = 0.05)
{
  # Calculate confidence level
  conf_lev <- (1 - alpha) * 100

  # Calculate critical value
  crit_val <- qnorm(1 - alpha / 2)

  # Calculate the sample proportion
  phat <- x / n

  # Calculate margin of error
  margin_error <- crit_val * sqrt(phat * (1 - phat) / n)

  # Calculate lower and upper bounds of confidence interval
  lower <- phat - margin_error
  upper <- phat + margin_error

  # Print the results in a user-friendly manner
  cat(
    "\nConfidence Level:",
    round(conf_lev, 4), "%",
    "\nMargin of Error:",
    round(margin_error, 4),
    "\nCritical Value:",
    round(crit_val, 4),
    "\nSample Proportion:",
    round(phat, 4),
    "\nLower Bound:",
    round(lower, 4),
    "\nUpper Bound:",
    round(upper, 4),
    "\n"
  )

  # Return the sample proportion, critical value, lower bound, and upper bound values as a named list for further use
  result <- list(
    sprop = phat,
    critval = crit_val,
    Lbound = lower,
    Ubound = upper
  )

  return(invisible(result))

}
