#' Relative Frequency Histogram
#'
#'
#' The function `rel_hist` computes a relative frequency histogram of given data values.

#' @usage rel_hist(data, bins, col, xlab, ylab, main, ybreaks)

#' @param data A numeric vector containing the data to be plotted.
#' @param bins *Optional*. The number of bins to use for the histogram. The default is 30.
#' @param col *Optional*. The fill color for the histogram bars.
#' @param xlab *Optional*. The label for the x-axis.
#' @param ylab *Optional*. The label for the y-axis.
#' @param main *Optional*. The main title for the plot.
#' @param ybreaks *Optional*. A numeric vector specifying where to place tick marks on the y-axis. If NULL, tick marks are automatically calculated.
#'
#' @importFrom graphics hist
#' @importFrom graphics rect
#'
#' @export

# Define the function rel_hist
rel_hist <-
  function(data,
           bins = 30,
           col = "steelblue",
           xlab = "Data",
           ylab = "Relative Frequencies",
           main = "Relative Frequency Histogram",
           ybreaks = NULL) {

    # Compute the histogram data without plotting
    hist_data <- hist(data, breaks = bins, plot = FALSE)

    # Calculate relative frequencies
    rel_freqs <- hist_data$counts / sum(hist_data$counts)

    # If ybreaks is not provided, calculate it based on max frequency. This ensures round numbers on y-axis.
    if (is.null(ybreaks)) {
      maxcount = max(rel_freqs)
      mag = -floor(log10(maxcount))
      k = 1
      if (maxcount < 0.4)
        k = 2
      if (maxcount < 0.2)
        k = 4
      mx = ceiling(k * maxcount * (10 ^ mag))
      ybreaks = c(0:mx) / (k * (10 ^ mag))
    }

    # Initialize the plot
    xlim_range <- range(hist_data$breaks)
    ylim_range <- c(0, max(ybreaks))
    plot(
      0,
      0,
      type = "n",
      xlim = xlim_range,
      ylim = ylim_range,
      xlab = xlab,
      ylab = ylab,
      main = main,
      yaxt = 'n'
    )
    axis(2, at = ybreaks, labels = ybreaks)

    # Add bars with relative frequencies
    bar_positions <- hist_data$mids
    bar_heights <- rel_freqs
    bar_widths <- diff(hist_data$breaks)[1]  # Assume equal bin widths

    for (i in seq_along(bar_positions)) {
      rect(
        bar_positions[i] - bar_widths / 2,
        0,
        bar_positions[i] + bar_widths / 2,
        bar_heights[i],
        col = col
      )
    }
  }
