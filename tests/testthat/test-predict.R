test_that("Predictions can be computed", {

  # A prediction will be calculated using a blblm() model
  # If the value is greater than 0, hopefully that means it's working correctly

  set.seed(10)


  # Compute a fit with blblm()
  blbFit <- blblm(mtcars, mpg ~ hp, m = 3, B = 10)


  expect_gt(predict.blblm(blbFit, mtcars[1, ]), 0)
})


test_that("Predictions are probably computed correctly", {

  # The prediction from a model made with lm() will be compared with
  # the confidence interval produced by blblm()


  set.seed(10)


  # Compute fits using lm() and blblm()
  regularFit <- lm(mpg ~ hp, mtcars)
  blbFit <- blblm(mtcars, mpg ~ hp, m = 3, B = 10)


  # The lm() prediction should be within the 99% confidence interval
  expect_gte(predict(regularFit, mtcars[1, ]), predict.blblm(blbFit, mtcars[1, ], TRUE, 0.99)[2])
  expect_lte(predict(regularFit, mtcars[1, ]), predict.blblm(blbFit, mtcars[1, ], TRUE, 0.99)[3])

})
