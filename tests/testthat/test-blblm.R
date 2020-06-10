test_that("useParallel = TRUE returns correct output", {

  set.seed(10)

  # Produce fits with blblm()
  blbFit <- blblm(mtcars, mpg ~ hp, B = 30)


  # Now, repeat the fitting with parallel functions
  future::plan(future::sequential)

  set.seed(10)

  blbFit2 <- blblm(mtcars, mpg ~ hp, B = 30, useParallel = TRUE)


  # Everything should be identical
  expect_identical(blbFit, blbFit2)

})


test_that("blblm() returns correctly formatted output", {

  # This one will check the formatting of the output.


  set.seed(10)


  # Produce fits with blblm()
  blbFit <- blblm(mtcars, mpg ~ hp, B = 3)


  # Their output names should be the same
  expect_equal(names(blbFit), c("estimates", "formula"))

})


test_that("useFreqs = FALSE returns correctly formatted output", {


  set.seed(10)


  # Produce fits with blblm()
  blbFit <- blblm(mtcars, mpg ~ hp, m =3, B = 15, useFreqs = FALSE)



  # Their formulas and names should be identical
  expect_identical(blbFit$formula, mpg ~ hp)
  expect_identical(names(blbFit$estimates), 1:3 %>% paste0())


})


test_that("useFreqs = FALSE returns accurate output", {


  # Produce fits with blblm()
  blbFit <- blblm(mtcars, mpg ~ hp + cyl, m = 1, B = 20, useFreqs = FALSE)


  # Compare the fit's first output vector (since they're all the same) with that of lm()
  lmFit <- lm(mpg ~ hp + cyl, mtcars)


  # Their data should be equivalent
  expect_equivalent(coef(blbFit), coef(lmFit))
  expect_equal(Adj.Rsq.blblm(blbFit), summary(lmFit)$adj.r.squared)
  expect_equal(Rsq.blblm(blbFit), summary(lmFit)$r.squared)
  expect_equal(sigma(blbFit), sigma(lmFit))


})


test_that("The new blblm functions still produce equivalent output as the original versions", {

  # Define the original blblm functions here
  # (This is going to be messy)



  old_blblm <- function(formula, data, m = 10, B = 5000) {
    data_list <- old_split_data(data, m)
    estimates <- map(
      data_list,
      ~ old_lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
    res <- list(estimates = estimates, formula = formula)
    class(res) <- "blblm"
    invisible(res)
  }

  old_split_data <- function(data, m) {
    idx <- sample.int(m, nrow(data), replace = TRUE)
    data %>% split(idx)
  }


  # compute the estimates
  old_lm_each_subsample <- function(formula, data, n, B) {
    replicate(B, old_lm_each_boot(formula, data, n), simplify = FALSE)
  }


  # compute the regression estimates for a blb dataset
  old_lm_each_boot <- function(formula, data, n) {
    freqs <- rmultinom(1, n, rep(1, nrow(data)))
    old_lm1(formula, data, freqs)
  }


  # estimate the regression estimates based on given the number of repetitions
  old_lm1 <- function(formula, data, freqs) {
    # drop the original closure of formula,
    # otherwise the formula will pick a wront variable from the global scope.
    environment(formula) <- environment()
    fit <- lm(formula, data, weights = freqs)
    list(coef = old_blbcoef(fit), sigma = old_blbsigma(fit))
  }


  # compute the coefficients from fit
  old_blbcoef <- function(fit) {
    coef(fit)
  }


  # compute sigma from fit
  old_blbsigma <- function(fit) {
    p <- fit$rank
    y <- model.extract(fit$model, "response")
    e <- fitted(fit) - y
    w <- fit$weights
    sqrt(sum(w * (e^2)) / (sum(w) - p))
  }


  old_print <- function(x, ...) {
    cat("blblm model:", capture.output(x$formula))
    cat("\n")
  }


  old_sigma <- function(object, confidence = FALSE, level = 0.95, ...) {
    est <- object$estimates
    sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
    if (confidence) {
      alpha <- 1 - level
      limits <- est %>%
        old_map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
        set_names(NULL)
      return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
    } else {
      return(sigma)
    }
  }


  old_coef <- function(object, ...) {
    est <- object$estimates
    old_map_mean(est, ~ old_map_cbind(., "coef") %>% rowMeans())
  }


  old_confint <- function(object, parm = NULL, level = 0.95, ...) {
    if (is.null(parm)) {
      parm <- attr(terms(object$formula), "term.labels")
    }
    alpha <- 1 - level
    est <- object$estimates
    out <- old_map_rbind(parm, function(p) {
      old_map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
    })
    if (is.vector(out)) {
      out <- as.matrix(t(out))
    }
    dimnames(out)[[1]] <- parm
    out
  }


  old_predict <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
    est <- object$estimates
    X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
    if (confidence) {
      old_map_mean(est, ~ old_map_cbind(., ~ X %*% .$coef) %>%
                 apply(1, old_mean_lwr_upr, level = level) %>%
                 t())
    } else {
      old_map_mean(est, ~ old_map_cbind(., ~ X %*% .$coef) %>% rowMeans())
    }
  }


  old_mean_lwr_upr <- function(x, level = 0.95) {
    alpha <- 1 - level
    c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
  }


  old_map_mean <- function(.x, .f, ...) {
    (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
  }


  old_map_cbind <- function(.x, .f, ...) {
    map(.x, .f, ...) %>% reduce(cbind)
  }


  old_map_rbind <- function(.x, .f, ...) {
    map(.x, .f, ...) %>% reduce(rbind)
  }



  # Compute blblm estimates
  set.seed(100)
  blbFit <- blblm(mtcars, mpg ~ hp * cyl, m = 3, B = 100)

  set.seed(100)
  oldFit <- old_blblm(mpg ~ hp * cyl, mtcars, m = 3, B = 100)


  # The formulas should be identical
  expect_identical(blbFit$formula, oldFit$formula)


  # The estimates should all be similar
  # (other than R^2 and Adj-R^2, which were added later)
  expect_equivalent(blbFit$estimates$`1`[[1]][1:2], oldFit$estimates$`1`[[1]])
  expect_equivalent(coef(blbFit), old_coef(oldFit))
  expect_equivalent(sigma(blbFit), old_sigma(oldFit))
  expect_equivalent(sigma(blbFit, confidence = TRUE), old_sigma(oldFit, confidence = TRUE))
  expect_equivalent(predict(blbFit, mtcars[1, ]), old_predict(oldFit, mtcars[1, ]))
  expect_equivalent(predict(blbFit, mtcars[1, ], confidence = TRUE), old_predict(oldFit, mtcars[1, ], confidence = TRUE))


  # Also test without any intercept (because old_confint() doesn't give an interval for "(Intercept)")
  set.seed(100)
  blbFit <- blblm(mtcars, mpg ~ 0 + hp * cyl, m = 3, B = 100)

  set.seed(100)
  oldFit <- old_blblm(mpg ~ 0 + hp * cyl, mtcars, m = 3, B = 100)


  expect_equivalent(confint(blbFit), old_confint(oldFit))


})