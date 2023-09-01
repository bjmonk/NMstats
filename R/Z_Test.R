#' Hypothesis Test for a Population Mean, Standard Deviation Known
#'
#' This function performs a hypothesis test about a population mean when the population standard deviation is known. P-values are calculated based on the normal distribution.
#'
#' @param xbar   The sample mean.
#' @param n      The sample size.
#' @param sigma  The population standard deviation.
#' @param mu0  The hypothesized mean in the null hypothesis.
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
#'@importFrom stats pnorm
#'
#' @note This function is appropriate when the population standard deviation is known. If the population standard deviation is not known, consider using the \code{\link{T_Test}} function, based on the Student's t-distribution.
#'
#' @export

Z_Test <-
  function(xbar,
           n,
           sigma,
           mu0,
           alt = "two")
  {
    # Check that alternate is one of the allowed values
    alt <- match.arg(alt, choices = c("left", "right", "two", "less", "greater", "two.sided"))

    # Map alternate names
    if (alt == "less") alt <- "left"
    if (alt == "greater") alt <- "right"
    if (alt == "two.sided") alt <- "two"

    # Calculate the test statistic
    z <- (xbar - mu0) / (sigma / sqrt(n))

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
    cat(
       "\nTest Statistic: z =",
      round(z, 4),
      "\nP-Value:",
      round(p_val, 4),
      "\n"
    )

    # Return the values as a named list for further use
    result <- list(z = z,
                   pvalue = p_val)

    return(invisible(result))
  }
