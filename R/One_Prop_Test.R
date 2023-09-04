#' One Sample Hypothesis Test for a Population Proportion
#'
#' This function performs a one-sample hypothesis test for a population proportion.
#'
#' @param x   The number of individuals of interest in the sample.
#' @param n   The sample size.
#' @param p0  The hypothesized proportion in the null hypothesis.
#' @param alt  A character string specifying the alternate hypothesis. Choices are "left", "right", or "two". If omitted, the default is "two". The choices "less", "greater", or "two.sided" are also accepted.
#'
#' @details The function will print the sample proportion, test statistic z, and the p-value.
#'
#' @return  The function returns, invisibly, a list containing:
#' \itemize{
#'       \item \code{sprop}: The sample proportion.
#'       \item \code{z}: The test statistic z.
#'       \item \code{pvalue}: The p-value corresponding to the test statistic.
#'     }
#' You can access these values with expressions like \code{res$sprop}, \code{res$z}, and \code{res$pvalue} where \code{res} is the result of the function.
#'
#' @importFrom stats pnorm
#'
#' @export



One_Prop_Test <- function(x, n, p0, alt = "two")
{
  # Check that alternate is one of the allowed values
  alt <- match.arg(alt, choices = c("left", "right", "two", "less", "greater", "two.sided"))

  # Map alternate names
  if (alt == "less") alt <- "left"
  if (alt == "greater") alt <- "right"
  if (alt == "two.sided") alt <- "two"

  # Calculate the sample proportion
  phat <- x / n

  # Calculate the test statistic
  z <- (phat - p0) / (sqrt(p0 * (1 - p0) / n))

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
  cat("Sample Proportion:", round(phat, 4),
      "\nTest Statistic: z =", round(z, 4),
      "\nP-Value:", round(p_val,4), "\n")

  # Return the values as a named list for further use
  result <- list(
    sprop = phat,
    z = z,
    pvalue = p_val
  )

  return(invisible(result))
}
