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
#' An integer value representing the number of permutations of `r` items chosen from `n`.
#'
#' @export

perms <- function(n, r) {
  if (r > n) {
    stop("r should not be greater than n")
  }
  factorial(n) / factorial(n - r)
}
