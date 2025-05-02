test_that("myncurve returns a list with correct elements", {
  result <- myncurve(mu = 0, sigma = 1, a = 1.5)

  expect_type(result, "list")
  expect_named(result, c("mu", "sigma", "a", "probability"))
  expect_true(is.numeric(result$probability))
})

test_that("myncurve calculates probability correctly for standard normal distribution", {
  result <- myncurve(mu = 0, sigma = 1, a = 0)
  expect_equal(result$probability, 0.5, tolerance = 1e-4)
})

test_that("myncurve handles a general normal distribution correctly", {
  result <- myncurve(mu = 10, sigma = 5, a = 10)
  expect_equal(result$probability, 0.5, tolerance = 1e-4)
})

test_that("myncurve returns probability close to 1 for large a", {
  result <- myncurve(mu = 0, sigma = 1, a = 5)
  expect_gt(result$probability, 0.99)
})

test_that("myncurve returns probability close to 0 for small a", {
  result <- myncurve(mu = 0, sigma = 1, a = -5)
  expect_lt(result$probability, 0.01)
})
