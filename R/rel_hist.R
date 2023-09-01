#' Relative Histogram
#'
#'
#' The function `rel_hist` computes a relative histogram of given data values.

#' @usage rel_hist(x, ...)
#' @param x   a vector of values for the relative histogram
#' @param ... further arguments and graphical parameters available in the `hist` function.
#'
#'
#'@importFrom graphics hist
#'

rel_hist <- function(x,
                          main = "Relative Histogram",
                          ylab = "Relative Frequencies",
                          ybreaks = NULL,
                          ...) {
  # Create a histogram without plotting it
  h <- hist(x, plot = FALSE, ...)

  # Convert counts to relative frequencies
  h$counts <- h$counts / sum(h$counts)

  # Calculate default y-axis breaks if not provided
  if (is.null(ybreaks)) {
    ybreaks <- seq(0, max(h$counts), length.out = 6)
  }

  # Filter out hist-specific arguments before calling plot
  plot_args <- list(...)
  unwanted_args <- c("breaks", "freq", "probability", "include.lowest", "right")
  plot_args <- plot_args[!(names(plot_args) %in% unwanted_args)]

  # Plot the relative histogram without the y-axis
  do.call(plot, c(list(h, main = main, ylab = ylab, yaxt = 'n'), plot_args))

  # Add y-axis with specified tick marks
  axis(side = 2, at = ybreaks, labels = round(ybreaks, 3))
}
