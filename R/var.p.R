#' Calculate the population variance
#'
#' The function `var.p` calculates the population variance of a data set `x`.
#'
#' @param x a vector of values
#'
#' @return
#' The population variance of `x`.
#'
#' @importFrom stats var
#' @export

var.p <- function(x) {
  # Check for population size of 1
  if (length(x) == 1) {
    return(0)
  }

  # Calculate the population variance
  result <- var(x) * ((length(x) - 1) / length(x))

  # Return the result
  return(result)
}
