test_that("Coefficients are computed correctly", {

  # The model coefficients from a model made with lm() will be compared with
  # the coefficients produced by blblm()'s lm1()


  set.seed(10)


  # Generate weights to use in lm() and lm1()
  freqs <- sample(10, nrow(mtcars), replace = TRUE) / 10


  # Compute fits using lm() and lm1()
  regularFit <- lm(mpg ~ hp + cyl, mtcars, weights = freqs)
  blbFit <- lm1(mtcars, mpg ~ hp + cyl, freqs)


  # Check that the confidence intervals are approximately equal
  # The total absolute difference between all four estimates
  # should be less than 5
  expect_equal(blbFit$coef, regularFit$coefficients)

})


test_that("Coefficient estimates are (hopefully) reasonable", {

  # The model coefficients from a model made with lm() will be compared with
  # the average coefficients produced by blblm()


  set.seed(10)


  # Compute fits using lm() and lm1()
  regularFit <- lm(mpg ~ hp + cyl, mtcars)
  blbFit <- blblm(mtcars, mpg ~ hp + cyl, B = 12)


  # Check that the total absolute differences between the
  # all three estimates should be less than 25
  expect_true(sum(abs(coef(blbFit) - coef(regularFit))) < 25)

})
