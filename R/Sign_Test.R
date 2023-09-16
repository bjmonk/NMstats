#' Sign Test for Testing a Median
#'
#' This function performs a nonparametric one-sample Sign Test for a population median when the population distribution is not necessarily normal. Critical values are included based on specified alpha levels and alternate hypotheses.
#'
#' @param sample A numeric vector representing the sample data.
#' @param m0 Hypothesized population median in the null hypothesis.
#' @param alpha The significance level for the hypothesis test (between 0 and 1, exclusive). Choices for one-tailed tests include 0.005, 0.01, 0.025, 0.05. Choices for two-tailed tests include 0.01, 0.02, 0.05, 0.10.
#' @param alt    A character string specifying the alternate hypothesis. Choices are "left", "right", or "two". If omitted, the default is "two". The choices "less", "greater", or "two.sided" are also accepted.
#'
#' @details The function calculates the test statistic, critical value, and determines whether to reject the null hypothesis. It provides detailed output including the null and alternate hypotheses, significance level, test statistic, critical value, and the test result.
#'
#' @import stats qnorm
#' @export


Sign_Test <- function(sample, m0, alpha, alt = "two.sided")
{
  # Define Table A.7 data directly in the function
  table_data <- data.frame(
    n = 5:25,
    `two.alpha_0.010` = c(NA, NA, NA, 0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5),
    `two.alpha_0.020` = c(NA, NA, NA, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5, 6),
    `two.alpha_0.050` = c(NA, NA, NA, 0, 1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 6, 6, 6, 6),
    `two.alpha_0.100` = c(0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5, 6, 6, 7, 7, 7),
    `one.alpha_0.005` = c(NA, NA, NA, 0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5),
    `one-alpha_0.010` = c(NA, NA, NA, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5, 6),
    `one-alpha_0.025` = c(NA, NA, NA, 0, 1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 6, 6, 6, 6),
    `one-alpha_0.050` = c(0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5, 6, 6, 7, 7, 7)
  )

  # Validate input

  if (!alt %in% c("left", "less", "right", "greater", "two", "two.sided")) {
    stop(
      "Error: 'alt' must be one of 'left', 'right', 'two.sided', 'less', 'greater', or 'two'"
    )
  }

  if (!is.numeric(sample) || !is.numeric(m0)) {
    stop("Error: 'sample' and 'm0' must be numeric")
  }

  if (alt %in% c("left", "less", "right", "greater") &&
      !alpha %in% c(0.005, 0.01, 0.025, 0.05)) {
    stop("Error: For one-tailed tests, 'alpha' must be one of 0.005, 0.01, 0.025, or 0.05")
  }


  if (alt %in% c("two", "two.sided") &&
      !alpha %in% c(0.01, 0.02, 0.05, 0.1)) {
    stop("Error: For two-tailed tests, 'alpha' must be one of 0.01, 0.02, 0.05, or 0.1")
  }

  # Step 1: State the null and alternate hypotheses
  null_hypothesis <- paste("H0: m =", m0)
  if (alt %in% c("left", "less")) {
    alternate_hypothesis <- paste("H1: m <", m0)
  } else if (alt %in% c("right", "greater")) {
    alternate_hypothesis <- paste("H1: m >", m0)
  } else {
    alternate_hypothesis <- paste("H1: m ≠", m0)
  }

  # Step 2: Choose a significance level
  significance_level <- alpha

  # Step 3: Count the number of plus and minus signs and total of both (n)
  n_plus <- sum(sample > m0)
  n_minus <- sum(sample < m0)
  x <- min(n_plus, n_minus)
  n <- n_plus + n_minus

  # Step 4: Compute the test statistic based on whether n <= 25 or n > 25
  if (n <= 25) {
    test_statistic <- x
  } else {
    test_statistic <- (x + 0.5 - n / 2) / sqrt(n / 4)
  }

  # Step 5: Determine whether to reject H0 using standard normal critical values
  if (n <= 25) {
    if (alt %in% c("left", "less", "right", "greater")) {
      alpha_vector_name <-
        paste("one.alpha_", formatC(alpha, format = "f", digits = 3), sep = "")
    } else {
      alpha_vector_name <-
        paste("two.alpha_", formatC(alpha, format = "f", digits = 3), sep = "")
    }

    # Use the selected alpha_vector_name to get the critical value
    critical_value <-
      table_data[table_data$n == n, alpha_vector_name]

    # Determine whether test statistic <= critical value for small sample sizes
    reject_null <- test_statistic <= as.numeric(critical_value)

  } else {
    if (alt %in% c("left", "less")) {
      critical_value <- qnorm(alpha)
      reject_null <- test_statistic <= critical_value
    } else if (alt %in% c("right", "greater")) {
      critical_value <- qnorm(1 - alpha)
      reject_null <- test_statistic >= critical_value
    } else {  # Two-tailed test
      critical_value_low <- -qnorm(1 - alpha / 2)
      critical_value_high <- qnorm(1 - alpha / 2)
      reject_null <- test_statistic <= critical_value_low || test_statistic >= critical_value_high
      critical_value <- c(critical_value_low, critical_value_high)
    }
  }


  # Print the results in a user-friendly manner
  cat(
    "Null Hypothesis:",
    null_hypothesis,
    "\nAlternate Hypothesis:",
    alternate_hypothesis,
    "\nSignificance Level:",
    round(significance_level, 5),
    "\nTest Statistic:",
    round(test_statistic, 5),
    "\nCritical Value:",
    round(critical_value, 5),
    "\nResult:",
    ifelse(
      reject_null,
      "Reject the Null Hypothesis",
      "Do Not Reject the Null Hypothesis"
    ),
    "\n"
  )

}
