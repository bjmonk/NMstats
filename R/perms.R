#' Compute the Number of Permutations of r Items Chosen From n
#'
#' The function calculates the number of ways to choose `r` items from a total of `n` items without replacement and where the order matters.
#'
#' It uses the formula:
#' \deqn{P(n, r) = \frac{n!}{(n - r)!}}
#'
#' @param n The total number of items.
#' @param r The number of items to choose from `n`.
#'
#' @return
#' An integer value representing the number of permutations of `r` items chosen from `n`. If invalid input values are given, an error message is returned.
#'
#' @export

perms <- function(n, r) {
  # Check if n and r are non-negative integers
  if (!is.numeric(n) ||
      !is.numeric(r) ||
      n != as.integer(n) || r != as.integer(r) || n < 0 || r < 0) {
    stop("Both n and r must be non-negative integers")
  }

  # Check if r > n
  if (r > n) {
    stop("r should not be greater than n")
  }

  # Calculate number of permutations
  factorial(n) / factorial(n - r)
}
