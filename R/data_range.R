#' Range of a Data Set
#'
#' This function computes the range of a data set `x`.
#'
#' @param x   a vector or values
#'
#' @return  The function returns the range of a data set, defined as the minimum subtracted from the maximum.

data_range <-
  function(x)
  {

  # Return the value
    result <- max(x) - min(x)

    return(result)
  }
