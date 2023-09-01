#' Compute the Number of Combinations of r Items Chosen From n
#'
#' The function calculates the number of ways to choose `r` items from a total of `n` items without replacement and where the order does not matter.
#'
#' It uses the formula:
#' \deqn{C(n, r) = \frac{n!}{r!(n - r)!}}
#'
#' @param n The total number of items.
#' @param r The number of items to choose from `n`.
#'
#' @return
#' An integer value representing the number of combinations of `r` items chosen from `n`.
#'

combs <- function(n, r) {
  if (r > n) {
    stop("r should not be greater than n")
  }
  factorial(n) / (factorial(r) * factorial(n - r))
}
