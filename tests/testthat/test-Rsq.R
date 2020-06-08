test_that("R^2 is computed correctly in lm1()", {


  set.seed(10)


  # Compute fits using both lm() and its blblm counterpart, lm1()
  regularFit <- lm(mpg ~ hp, mtcars)
  blbFit <- lm1(mtcars, mpg ~ hp, freqs = replicate(nrow(mtcars), 1))


  # Their R^2 values should be the same
  expect_equal(blbFit$Rsq, summary(regularFit)$r.squared)

})


test_that("R^2 is probably computed correctly", {

  # The R^2 from lm() is used, and the test checks that it
  # is within the range of the blblm() R^2's confidence interval


  set.seed(10)


  # Compute fits using lm() and blblm()
  regularFit <- lm(mpg ~ hp, mtcars)
  blbFit <- blblm(mtcars, mpg ~ hp, m = 3, B = 5)


  # The lm() R^2 should be within the 99% confidence interval
  expect_gte(summary(regularFit)$r.squared, Rsq.blblm(blbFit, TRUE, level = 0.99)[2])
  expect_lte(summary(regularFit)$r.squared, Rsq.blblm(blbFit, TRUE, level = 0.99)[3])

})
