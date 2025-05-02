#' @title Normal Curve with Shaded Probability
#'
#' @param mu A numeric value representing the mean of the normal distribution.
#' @param sigma A positive numeric value representing the standard deviation of the normal distribution.
#' @param a A numeric value representing the cutoff point for shading and probability calculation.
#'
#' @return A list containing the mean, standard deviation, cutoff value, and the probability.
#' @export
#'
#' @examples
#' \dontrun{myncurve(mu = 0, sigma = 1, a = 1.5)}
myncurve <- function(mu, sigma, a) {
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma),
        ylab = "Density", main = paste("Normal Curve (mu =", mu, ", sigma =", sigma, ")"))

  xcurve <- seq(mu - 3 * sigma, a, length = 1000)
  ycurve <- dnorm(xcurve, mean = mu, sd = sigma)

  polygon(c(mu - 3 * sigma, xcurve, a), c(0, ycurve, 0), col = "blue", border = NA)

  prob <- round(pnorm(a, mean = mu, sd = sigma), 4)

  return(list(mu = mu, sigma = sigma, a = a, probability = prob))
}
