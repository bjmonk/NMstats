#' Signed Rank Test for Testing a Median Difference
#'
#' This function performs the nonparametric Signed Rank Test for testing whether there is a difference between the medians of two populations, when the data are in the form of paired samples.
#'
#' @param sample1 A numeric vector representing the first sample data.
#' @param sample2 A numeric vector representing the second sample data.
#' @param alpha The significance level for the hypothesis test (between 0 and 1, exclusive).
#'
#' @details
#' The Signed Rank Test is a nonparametric test that assesses whether there is a significant difference between the medians of two paired samples. The null hypothesis states that the median difference is zero, while the alternate hypothesis suggests a non-zero median difference.
#'
#' The function calculates the test statistic based on the absolute values of the signed rank differences between paired observations. It then compares the test statistic to critical values obtained from Table A.8 for different significance levels and sample sizes.
#'
#' If the test statistic falls within the critical region, the function will reject the null hypothesis, indicating a significant difference between the medians.
#'
#' @export

Signed_Rank_Test <- function(sample1, sample2, alpha) {
  # Check for valid alpha value
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("Error: 'alpha' must be a numeric value between 0 and 1.")
  }

  # Check for equal sample sizes
  if (length(sample1) != length(sample2)) {
    stop("Error: Sample sizes must be equal.")
  }


  # Define Table A.8 from Navidi/Monk text
  Table_A8 <- data.frame(
    n = 1:30,
    alpha_0.10 = c(
      rep(NA, 4),
      1,
      2,
      4,
      6,
      8,
      11,
      14,
      17,
      21,
      26,
      30,
      36,
      41,
      47,
      54,
      60,
      68,
      75,
      83,
      92,
      101,
      110,
      120,
      130,
      141,
      152
    ),
    alpha_0.05 = c(
      rep(NA, 5),
      1,
      2,
      4,
      6,
      8,
      11,
      14,
      17,
      21,
      25,
      30,
      35,
      40,
      46,
      52,
      59,
      66,
      73,
      81,
      90,
      98,
      107,
      117,
      127,
      137
    ),
    alpha_0.02 = c(
      rep(NA, 6),
      0,
      2,
      3,
      5,
      7,
      10,
      13,
      16,
      20,
      24,
      28,
      33,
      38,
      43,
      49,
      56,
      62,
      69,
      77,
      85,
      93,
      102,
      111,
      120
    ),
    alpha_0.01 = c(
      rep(NA, 7),
      0,
      2,
      3,
      5,
      7,
      10,
      13,
      16,
      19,
      23,
      28,
      32,
      37,
      43,
      49,
      55,
      61,
      68,
      76,
      84,
      92,
      100,
      109
    )
  )

  # Define null and alternate hypotheses
  null_hypothesis <- "md = 0"
  alternate_hypothesis <- "md is not equal to 0"

  # Identify significance level and sample size
  sig_level <- alpha
  n <- length(sample1)

  # Compute the absolute values of the differences for the matched pairs and assign NA to the 0's
  abs_differences <- abs(sample1 - sample2)
  abs_differences[abs_differences == 0] <- NA

  # Rank the absolute values (ignoring differences of 0)
  ranks <-
    rank(round(abs_differences, 15),
         ties.method = "average",
         na.last = "keep")
  ranks[abs_differences == 0] <- NA


  # Assign plus or minus to ranks
  signed_ranks <- sign(sample1 - sample2) * ranks


  # Sum of positive ranks and sum of negative ranks
  sum_positive_ranks <-
    sum(signed_ranks[signed_ranks > 0], na.rm = TRUE)
  sum_negative_ranks <-
    sum(signed_ranks[signed_ranks < 0], na.rm = TRUE)

  # Compute test statistic
  S <- min(abs(sum_positive_ranks), abs(sum_negative_ranks))

  # Get column name from Table A8
  alpha_vector_name <-
    paste("alpha_", formatC(sig_level, format = "f", digits = 2), sep = "")
  if (!alpha_vector_name %in% colnames(Table_A8)) {
    stop("Error: 'alpha' value is not available in Table_A8.")
  }

  # Use the column name to get the critical value based on alpha and n
  critical_value <- Table_A8[Table_A8$n == n, alpha_vector_name]

  # Determine whether test statistic <= critical value
  reject_null <- !is.na(critical_value) && (S <= critical_value)

  # Print the results in a user-friendly manner
  cat(
    "Null Hypothesis:",
    null_hypothesis,
    "\nAlternate Hypothesis:",
    alternate_hypothesis,
    "\nSignificance Level:",
    round(sig_level, 5),
    "\nTest Statistic:",
    round(S, 5),
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
