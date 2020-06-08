test_that("Confidence intervals are (probably) computed correctly", {

  # The confidence interval from a model made with lm() will be compared with
  # the confidence interval produced by blblm()


  set.seed(10)

  # Compute fits using lm() and blblm()
  regularFit <- lm(mpg ~ hp, mtcars)
  blbFit <- blblm(mtcars, mpg ~ hp, B = 15)


  # Check that the confidence intervals are approximately equal
  # The total absolute difference between all four estimates
  # should be less than 5
  expect_true(sum(abs(confint(regularFit) - confint(blbFit))) < 5)

})