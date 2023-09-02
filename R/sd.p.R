#' Calculate the population standard deviation
#'
#' The function `sd.p` calculates the population standard deviation of a data set `x`.
#'
#' @param x a vector of values
#'
#' @return
#' The population standard deviation of `x`.
#'
#' @importFrom stats var
#' @export

sd.p <- function(x) {
  # Calculate the population standard deviation
  result <- sqrt(var(x)*((length(x) - 1) / length(x) ))

  # Return the result
  return(result)
  }
