% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rel_hist.R
\name{rel_hist}
\alias{rel_hist}
\title{Relative Frequency Histogram}
\usage{
rel_hist(data, bins, col, xlab, ylab, main, ybreaks)
}
\arguments{
\item{data}{A numeric vector containing the data to be plotted.}

\item{bins}{\emph{Optional}. The number of bins to use for the histogram. The default is 30.}

\item{col}{\emph{Optional}. The fill color for the histogram bars.}

\item{xlab}{\emph{Optional}. The label for the x-axis.}

\item{ylab}{\emph{Optional}. The label for the y-axis.}

\item{main}{\emph{Optional}. The main title for the plot.}

\item{ybreaks}{\emph{Optional}. A numeric vector specifying where to place tick marks on the y-axis. If NULL, tick marks are automatically calculated.}
}
\description{
The function \code{rel_hist} computes a relative frequency histogram of given data values.
}
