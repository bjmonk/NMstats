#' Outlier Bounds using IQR Method
#'
#' This function computes the lower and upper outlier boundaries using the IQR method.
#'
#'
#' @param x   a vector or values
#'
#' @details
#' The lower outlier boundary is defined by `Q1 - 1.5(IQR)` and the upper outlier boundary is defined by `Q3 + 1.5(IQR)`. The function will print the results.
#'
#' @return  The function returns, invisibly, a list containing:
#' \itemize{
#'       \item \code{Lower.bound}: The lower outlier bound.
#'       \item \code{Upper.bound}: The upper outlier bound.}

#' You can access these values with expressions like \code{res$Lower.bound} and \code{res$Upper.bound} where \code{res} is the result of the function.
#'
#'@importFrom graphics axis
#'@importFrom stats quantile

outlier_bounds <-
  function(x)
  {
    # Compute quartiles
    Q1 <- unname(quantile(x, 0.25))
    Q3 <- unname(quantile(x, 0.75))

    # Compute interquartile range
    IQR <- Q3 - Q1

    # Compute outlier bound
    Lower.bound <- round(Q1 - 1.5*IQR, 4)
    Upper.bound <- round(Q3 + 1.5*IQR, 4)

    # Print the results in a user-friendly manner
    cat(
      "\nLower Outlier Bound:",
      Lower.bound,
      "\nUpper Outlier Bound:",
      Upper.bound,
      "\n"
    )
  # Return the values
    result <- list(Lower.bound = Lower.bound,
                   Upper.bound = Upper.bound)

    return(invisible(result))
  }

