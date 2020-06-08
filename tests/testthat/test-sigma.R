test_that("Sigma is computed correctly", {

  # The sigma from a model made with lm() will be compared with
  # the sigma produced by blblm()'s lm1()


  set.seed(10)


  # Generate weights to use in lm() and lm1()
  freqs <- sample(10, nrow(mtcars), replace = TRUE)


  # Compute fits using lm() and lm1()
  regularFit <- lm(mpg ~ hp + cyl, mtcars, weights = freqs / sum(freqs))
  blbFit <- lm1(mtcars, mpg ~ hp + cyl, freqs)


  # Check that the sigma values are approximately equal
  # The absolute difference should be less than 5
  expect_true(abs(blbFit$sigma - sigma(regularFit)) < 6)

})


test_that("Sigma estimates are (hopefully) reasonable", {

  # The sigma from a model made with lm() will be compared with
  # the average sigma interval produced by blblm()


  set.seed(10)


  # Compute fits using lm() and lm1()
  regularFit <- lm(mpg ~ hp + cyl, mtcars)
  blbFit <- blblm(mtcars, mpg ~ hp + cyl, B = 15)


  # Check that the sigma from lm() is within the 99% confidence
  # interval produced by blblm()
  expect_gte(sigma(regularFit), sigma.blblm(blbFit, TRUE, level = 0.99)[2])

})
