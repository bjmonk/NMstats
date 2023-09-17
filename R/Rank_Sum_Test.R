#' Rank Sum Test for Comparing the Medians of Two Populations
#'
#' This function performs the nonparametric Rank Sum Test for comparing the medians of populations
#'
#' @param sample1 A numeric vector representing the first sample data.
#' @param sample2 A numeric vector representing the second sample data.
#' @param alpha The significance level for the hypothesis test (between 0 and 1, exclusive).
#' @param alt A character string specifying the alternate hypothesis. Choices are "left", "right", "two", "less", "greater", or "two.sided". If omitted, the default is "two.sided".
#'
#' @details The function provides detailed output including the null and alternate hypotheses, significance level, test statistic, p-value, and the test result.
#'
#' @importFrom stats pnorm
#' @export

Rank_Sum_Test <- function(sample1, sample2, alpha, alt = "two.sided") {
  # Check that alternate is one of the allowed values
  alt <- match.arg(alt, choices = c("left", "right", "two", "less", "greater", "two.sided"))

  # Map alternate names
  if (alt == "less") alt <- "left"
  if (alt == "greater") alt <- "right"
  if (alt == "two.sided") alt <- "two"

  # Define null and alternate hypotheses
  null_hypothesis <- "m1 = m2"

  if (alt == "left") {
    alternate_hypothesis <- "m1 < m2"
  } else if (alt == "right") {
    alternate_hypothesis <- "m1 > m2"
  } else {
    alternate_hypothesis <- "m1 is not equal to m2"
  }

  # Combine the two samples
  combined_sample <- c(sample1, sample2)
  ranks <- rank(combined_sample, ties.method = "average", na.last = "keep")

  # Split the ranks back into their original samples
  ranks1 <- ranks[1:length(sample1)]
  ranks2 <- ranks[(length(sample1) + 1):(length(sample1) + length(sample2))]

  # Determine which sample is smaller
  if (length(sample1) < length(sample2)) {
    S <- sum(ranks1)
    n1 <- length(sample1)
    n2 <- length(sample2)
  } else {
    S <- sum(ranks2)
    n1 <- length(sample2)
    n2 <- length(sample1)
  }

  # Calculate mean and standard deviation of S
  mean_S <- n1 * (n1 + n2 + 1) / 2
  sd_S <- sqrt((n1 * n2 * (n1 + n2 + 1)) / 12)

  # Compute the z-test statistic
  z <- (S - mean_S) / sd_S

  # Compute the p-value based on the alternative hypothesis
  if (alt == "left") {
    p_value <- pnorm(z)
  } else if (alt == "right") {
    p_value <- 1 - pnorm(z)
  } else {
    p_value <- 2 * min(pnorm(z), 1 - pnorm(z))
  }

  # Make a decision to reject or not reject the null hypothesis
  reject_null <- p_value <= alpha

  # Print the results in a user-friendly manner

  cat(
    "Null Hypothesis:", null_hypothesis,
    "\nAlternate Hypothesis:", alternate_hypothesis,
    "\nSignificance Level:", round(alpha, 5),
    "\nTest Statistic:", round(z, 5),
    "\nP-Value:", round(p_value, 5),
    "\nResult:", ifelse(reject_null, "Reject the Null Hypothesis", "Do Not Reject the Null Hypothesis"),
    "\n"
  )
}

