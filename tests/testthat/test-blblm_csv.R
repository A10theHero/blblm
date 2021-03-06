test_that("blblm_csv() and blblm() return similar output", {


  # Ensure that the two functions are equivalent

  set.seed(10)


  # Get the splits that will be used in blblm()
  separatedData <- mtcars %>% split_data(m = 3)


  # Make CSV files with those splits
  invisible(1:3 %>% map(function(i) {
    separatedData[[i]] %>% write_csv(paste0("temp", i, ".csv"))
  }))


  # Starting from the top,
  set.seed(10)


  # Produce estimates with blblm()
  blbFit <- blblm(mtcars, mpg ~ hp, m = 3, B = 3)


  # Restart again
  set.seed(10)


  # Advance the RNG by the same amount as split_data() does in blblm()
  separatedData <- mtcars %>% split_data(m = 3)


  # This ensures that the same weights are chosen here
  blbFit2 <- paste0("temp", 1:3, ".csv") %>% blblm_csv(mpg ~ hp, B = 3)


  # The two results should be almost exactly the same
  # Their values are all identical
  # Only the names within 'estimates' differ
  expect_equal(names(blbFit), names(blbFit2))
  expect_equal(blbFit$estimates[[1]][[1]], blbFit2$estimates[[1]][[1]])
  expect_equal(blbFit$estimates[[2]][[3]], blbFit2$estimates[[2]][[3]])
  expect_equal(blbFit$formula, blbFit2$formula)

})


test_that("useParallel = TRUE works with blblm_csv()", {


  # Compare the results with blblm(useParallel = TRUE)


  set.seed(10)


  # Get the splits that will be used in blblm()
  separatedData <- mtcars %>% split_data(m = 3)


  # Make CSV files with those splits
  invisible(1:3 %>% map(function(i) {
    separatedData[[i]] %>% write_csv(paste0("temp", i, ".csv"))
  }))

  future::plan(future::sequential)

  set.seed(10)
  blbFit <- blblm(mtcars, mpg ~ hp, m = 3, B = 3, useParallel = TRUE)


  # Starting again,
  set.seed(10)


  # Advance the RNG
  separatedData <- mtcars %>% split_data(m = 3)


  blbFit2 <- paste0("temp", 1:3, ".csv") %>% blblm_csv(mpg ~ hp, B = 3, useParallel = TRUE)


  # The results should be the same
  # Their values are all identical
  # Only the names within 'estimates' differ
  expect_equal(names(blbFit), names(blbFit2))
  expect_equal(blbFit$estimates[[1]][[1]], blbFit2$estimates[[1]][[1]])
  expect_equal(blbFit$estimates[[2]][[3]], blbFit2$estimates[[2]][[3]])
  expect_equal(blbFit$formula, blbFit2$formula)

})


test_that("useFreqs = FALSE works with blblm_csv()", {


  # Compare the results with blblm(useFreqs = FALSE)


  set.seed(10)


  # Get the splits that will be used in blblm()
  separatedData <- mtcars %>% split_data(m = 1)


  # Make CSV a file with those splits
  invisible(1 %>% map(function(i) {
    separatedData[[i]] %>% write_csv(paste0("temp", i, ".csv"))
  }))


  set.seed(10)
  blbFit <- blblm(mtcars, mpg ~ hp, m = 1, B = 3, useFreqs = FALSE)


  # Starting again,
  set.seed(10)


  # Advance the RNG
  separatedData <- mtcars %>% split_data(m = 1)


  blbFit2 <- paste0("temp", 1:3, ".csv") %>% blblm_csv(mpg ~ hp, B = 3, useFreqs = FALSE)


  # Their values are all identical
  expect_equal(names(blbFit), names(blbFit2))
  expect_equal(blbFit$estimates[[1]][[1]], blbFit2$estimates[[1]][[1]])
  expect_equal(blbFit$estimates[[1]][[3]], blbFit2$estimates[[1]][[3]])
  expect_equal(blbFit$formula, blbFit2$formula)

})