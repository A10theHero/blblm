test_that("blblm() returns correctly formatted output", {

  # The specific output values are tested sufficiently in other
  # test files. This one will check the formatting of the output.


  set.seed(10)


  # Produce fits with blblm()
  blbFit <- blblm(mtcars, mpg ~ hp, B = 3)


  # Their output names should be the same
  expect_equal(names(blbFit), c("estimates", "formula"))

})