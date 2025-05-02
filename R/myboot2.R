#' @title Bootstrap Confidence Interval Function
#' @description
#' This function performs bootstrap resampling to compute a confidence interval for a specified statistic.
#'
#' @param iter Number of bootstrap iterations. Default is 10000.
#' @param x A numeric vector of data values to be bootstrapped.
#' @param fun A function used to compute the statistic of interest.
#' @param alpha Significance level for the confidence interval. Default is 0.05.
#' @param cx Character expansion factor for text size in the plot. Default is 1.5.
#' @param ... Additional graphical parameters to pass to the hist function
#'
#' @return An invisible list containing ci, fun, and x
#' @export
#'
#' @examples
#' \dontrun{myboot2(iter = 10000, x = x, alpha = 0.05, fun = "mean", xlab = "Bootstrapped Means")}
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...){  #Notice where the ... is repeated in the code
  n <- length(x)   #sample size

  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nr = n,nc = iter, byrow = TRUE)

  # xstat is a vector and will have iter values in it
  xstat <- apply(rs.mat, 2, fun)

  # Nice way to form a confidence interval
  ci <- quantile(xstat, c(alpha/2, 1 - alpha/2))

  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para <- hist(xstat, freq = FALSE, las = 1,
               main = paste("Histogram of Bootstrap sample statistics", "\n", "alpha=", alpha," iter=", iter, sep=""), ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat <- matrix(x, nr = length(x), nc = 1, byrow = TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte <- apply(mat, 2, fun)

  # Vertical line
  abline(v = pte, lwd = 3, col = "Black")

  #Make the segment for the ci
  segments(ci[1], 0, ci[2], 0, lwd = 4)
  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = cx)

  # plot the point estimate 1/2 way up the density
  text(pte, max(para$density)/2, round(pte, 2), cex = cx)

  # Some output to use if necessary
  invisible(list(ci = ci,fun = fun,x = x))
}
