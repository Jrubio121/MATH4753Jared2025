#' @title Find Optimal Number of Tickets to Sell
#'
#' @param N An integer representing the total number of available seats.
#' @param gamma A numeric value representing the acceptable probability of overbooking.
#' @param p A numeric value representing the probability that a single ticketed passenger shows up.
#'
#' @return A list containing:
#' \item{nd}{Optimal number of tickets sold in the discrete case.}
#' \item{nc}{Optimal number of tickets sold in the continuous case.}
#' \item{N}{Total number of seats available.}
#' \item{p}{Probability of a passenger showing up.}
#' \item{gamma}{Acceptable probability of overbooking.}
#' @export
#'
#' @examples
#' \dontrun{ntickets(N = 400, gamma = 0.02, p = 0.95)}
ntickets <- function(N, gamma, p) {

  # Compute objective function for discrete case
  compute_discrete_objective <- function(x) {
    1 - gamma - pbinom(N, x, p, lower.tail = TRUE)
  }

  # Compute objective function for continuous case
  compute_continuous_objective <- function(x) {
    1 - gamma - pnorm(N + 0.5, x * p, sqrt(x * p * (1 - p)), lower.tail = TRUE)
  }

  # Define x values for evaluation
  x_vals <- seq(N, N + N / 10, by = 1)

  # Compute discrete objective values
  y_discrete <- compute_discrete_objective(x_vals)

  # Find first non-negative value as root
  root_discrete <- x_vals[min(which(y_discrete >= 0))]

  # Plot discrete case
  plot(x_vals, y_discrete, type = "b",
       main = "",
       xlab = "x_vals",
       ylab = "y_discrete")

  title(main = sprintf("Objective vs. n to Find Optimal Tickets Sold (%d) N = %d, gamma = %.2f (Discrete)", root_discrete, N, gamma), cex.main = 0.9, line = 1)

  # Add reference lines
  abline(h = 0, v = root_discrete, col = "red")
  points(x_vals, compute_discrete_objective(x_vals), col = "blue", pch = 19)

  # Compute continuous root
  root_continuous <- uniroot(compute_continuous_objective, interval = range(x_vals))$root

  # Compute continuous objective values
  y_continuous <- compute_continuous_objective(x_vals)

  # Plot continuous case
  plot(x_vals, y_continuous, type = "l",
       main = "",
       xlab = "x_vals",
       ylab = "y_continuous")

  title(main = sprintf("Objective vs. n to Find Optimal Tickets Sold (%.2f) N = %d, gamma = %.2f (Continuous)",root_continuous, N, gamma), cex.main = 0.9, line = 1)

  # Add reference lines
  abline(h = 0, v = root_continuous, col = "blue")

  # Return results
  result <- list(nd = root_discrete, nc = root_continuous, N = N, p = p, gamma = gamma)
  print(result)
}
