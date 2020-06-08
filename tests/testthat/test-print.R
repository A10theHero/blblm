test_that("Console output is correct", {


  # The formula to output
  formu <- mpg ~ hp * cyl


  # Compute a fit using blblm()
  blbFit <- blblm(mtcars, formu, B = 10)


  # Check that the print statements are the same
  blbOutput <- capture.output(print.blblm(blbFit))


  testOutput <- "blblm model: mpg ~ hp * cyl"


  # blbOutput may have an environment tag at the end
  # But other than that, the two strings should be identical
  expect_identical(substr(blbOutput, 1, nchar(testOutput)), testOutput)

})
