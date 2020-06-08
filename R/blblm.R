#' @import purrr
#' @import stats
#' @import furrr
#' @import readr
#' @import future
#' @importFrom magrittr %>%
#' @aliases NULL
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))


#' blblm() with CSV files
#'
#' Rather than use a data frame variable as input, the data is read in from specified filenames.
#' Since the data is already split, a shortened version of blblm() is applied separately to the data within each file.
#'
#' @param filenames List (or vector) of CSV files
#' @param formu Regression formula
#' @param B Number of bootstraps per subsample
#' @param useParallel Boolean for using parallel functions (future::plan() must be set before using this)
#' @param useFreqs Boolean for generating frequencies for each row of data (used in weighted linear regression)
#'
#' @return A list with (1) elements separated by filename and bootstrap trial and (2) the model formula
#' (Note: The filenames are used as list names in the output).
#' Within each trial, the output includes the model coefficients, sigma, R^2, Adjusted R^2, and Weighted R^2 (if weights are used).
#' @export
blblm_csv <- function(filenames, formu, B = 5000, useParallel = FALSE, useFreqs = TRUE) {


  # If parallel functionality is enabled:
  if (useParallel) {

    # Get the total number of data rows
    nrowTot <- filenames %>%
      furrr::future_map(nrowGetter) %>%
      base::unlist() %>% base::sum()


    # Use furrr's future_map() to apply lm_each_subsample() to each file's data
    est <- filenames %>%
      furrr::future_map(blblm_csv_map_func, formu = formu,
                        totN = nrowTot, boot = B, useFreqs = useFreqs)


  } else {

  # If useParallel is FALSE
  # Use map() instead of future_map()


    # Get the total number of data rows
    nrowTot <- filenames %>%
      purrr::map(nrowGetter) %>%
      base::unlist() %>% base::sum()

    est <- filenames %>%
      purrr::map(blblm_csv_map_func, formu = formu,
                 totN = nrowTot, boot = B, useFreqs = useFreqs)

  }


  # Use the filenames as names for the list entries
  base::names(est) <- filenames


  # Return both the estimates and the formula
  res <- base::list("estimates" = est, "formula" = formu)


  # Also apply the "blblm" class to the variable
  base::class(res) <- "blblm"


  # Return the results
  base::invisible(res)


}


nrowGetter = function(f) {

  # Read in the file
  # and return the number of rows

  readr::read_csv(f, col_types = cols()) %>%
    base::nrow()

}


blblm_csv_map_func = function(f, formu, totN, boot, useFreqs) {

  # Read the CSV file to get data
  inputData <- readr::read_csv(f, col_types = cols())


  # Apply lm_each_subsample()
  lm_each_subsample(data = inputData, formu = formu,
                    n = totN, B = boot,
                    useFreqs = useFreqs)
}


#' Linear modeling with a bag of little bootstraps
#'
#' This will produce mutliple Linear Regression models by splitting the data and using little bootstraps.
#'
#' @param dat Data Frame (or equivalent)
#' @param formu Regression formula
#' @param m Number of subsamples
#' @param B Number of bootstraps per subsample
#' @param useParallel Boolean for using parallel functions (future::plan() must be set before using this)
#' @param useFreqs Boolean for generating frequencies for each row of data (used in weighted linear regression)
#'
#' @return A list with (1) elements separated by filename and bootstrap trial and (2) the model formula
#' (Note: The filenames are used as list names in the output).
#' Within each trial, the output includes the model coefficients, sigma, R^2, Adjusted R^2, and Weighted R^2 (if weights are used)
#' @export
#' @examples
#' blblm(dat = mtcars, formu = mpg ~ wt * hp, m = 5, B = 91)
#' blblm(dat = mtcars, formu = mpg ~ wt * hp, m = 3, B = 100, useParallel = TRUE)
blblm <- function(dat, formu, m = 10, B = 5000, useParallel = FALSE, useFreqs = TRUE) {


  # Randomly sample the data to make several data frames
  data_list <- dat %>% split_data(m)


  # If parallel functionality is used
  if (useParallel == TRUE) {


   # Use future_map() to apply linear regression to each bootstrap sample
   estimates <- furrr::future_map(data_list,
                            ~ lm_each_subsample(data = ., formu = formu,
                                                n = base::nrow(dat), B = B,
                                                useFreqs = useFreqs))

  } else {

    # If useParallel is FALSE


    # Use map() instead of future_map()
    estimates <- purrr::map(data_list,
                            ~ lm_each_subsample(data = ., formu = formu,
                                                n = base::nrow(dat), B = B,
                                                useFreqs = useFreqs))
  }


  # Compile the results with the formula into a list
  res <- base::list("estimates" = estimates, "formula" = formu)


  # Apply the "blblm" classname to the result
  base::class(res) <- "blblm"


  # Return the result
  base::invisible(res)

}


# Split data into m parts of approximately equal sizes
split_data <- function(dat2, m) {


  # Assign each row of data to a subsample
  idx <- base::sample.int(m, base::nrow(dat2), replace = TRUE)


  # If there's any subsample with only one data point, resample the data
  # (0 rows is okay because the number of subsamples will reduce automatically)
  while (base::min(base::table(idx)) == 1) {
    idx <- base::sample.int(m, base::nrow(dat2), replace = TRUE)
  }


  # Return the split data list
  dat2 %>% base::split(idx)


}


# Compute the regression estimates
lm_each_subsample <- function(data, formu, n, B, useFreqs) {


  # Apply linear regression to each bootstrap sample
  base::replicate(B, lm_each_boot(data, formu, n, useFreqs), simplify = FALSE)

}


# Compute the regression estimates for a blb dataset
lm_each_boot <- function(data, formu, n, useFreqs) {


  # If useFreqs is TRUE
  # Randomly generate weights
  if (useFreqs) {

    # Generate numbers, nrow(entireSample) times
    # The numbers can be between 1 and nrow(subsample)
    # The tallied frequencies for those numbers are used
    freqs <- stats::rmultinom(1, n, base::rep(1, base::nrow(data)))

  # Otherwise, set all weights to 1
  } else {

    freqs <- base::replicate(base::nrow(data), 1)

  }


  # Apply (weighted) linear regression
  lm1(data, formu, freqs)
}


# Get regression estimates for each iteration
lm1 <- function(data, formul, freqs) {


  # y is in the first column of model.frame()
  y <- stats::model.frame(formul, data)[1] %>% as.matrix()


  # The intercept and prediction variables are gathered
  # using model.matrix()
  x <- stats::model.matrix(formul, data)


  # Make sure that the number of rows in x equals the number of rows in y
  if (base::nrow(x) != base::nrow(y)) {
    base::stop("The number of x-values does not equal the number of y-values")
  }


  # Check if an intercept is included in x
  # (Affects the formulas used in the calculations)
  if ("(Intercept)" %in% base::colnames(x)) {
    useIntercept = TRUE
  } else {
    useIntercept = FALSE
  }


  # Apply weighted LM
  fit <- fastLM(x, y,
         freqs %>% base::unlist() %>% base::as.numeric() %>%
           base::diag(., nrow = base::length(.), ncol = base::length(.)),
         useIntercept)


  # Give the coefficients their names
  base::names(fit$coef) = base::colnames(x)


  # Return the results
  fit
}


#' Print the model formula
#'
#' The formula will be printed for a "blblm" variable
#'
#' @param x An object with class "blblm"
#' @param ... Included for method flexibility
#'
#' @export
#' @method print blblm
print.blblm <- function(x, ...) {


  # Print the formula of the variable
  base::cat("blblm model:", utils::capture.output(x$formula), "\n")

}


#' Get sigma
#'
#' Get an estimate for the Residual Standard Deviation, with the option of a confidence interval
#'
#' @param object A variable with the class "blblm"
#' @param confidence Boolean for whether a confidence interval should be reported
#' @param level The significance level for a confidence interval (1 - alpha)
#' @param ... Included for method flexibility
#'
#' @return A numerical estimate for sigma (or a vector with a confidence interval for sigma)
#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {

  # Use the sigma estimates from every variable
  sigma <- map_mean(object$estimates, ~ map_cbind(., "sigma") %>% base::rowMeans())


  # If a confidence interval should be reported
  if (confidence) {

    # Get alpha based on the level
    alpha <- 1 - level

    # Compute the limits of the interval
    limits <- object$estimates %>%
      map_mean(~ stats::quantile(purrr::map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)

    # Return a named vector with the results
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))


  } else {

    # Otherwise, just return the sigma estimate
    return(sigma)

  }
}


#' Get the model coefficients
#'
#' Get an estimate for the coefficients
#'
#' @param object A variable with the class "blblm"
#' @param ... Included for method flexibility
#'
#' @return A vector of average values for the linear model's coefficients
#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {

  # Compute the means of the coefficients from each result

  map_mean(object$estimates, ~ map_cbind(., "coef") %>% base::rowMeans())

}


#' Get a confidence interval
#'
#' Produce a confidence interval for some or all of the model c0efficients (the latter by default)
#'
#' @param object A variable with the class "blblm"
#' @param parm A vector or list of the names of the variables for which an interval will be constructed.
#' Exact names (as output by stats::terms()) must be used. For intercepts, use "(Intercept)".
#' @param level The significance level for the confidence interval (1 - alpha)
#' @param ... Included for method flexibility
#'
#' @return A matrix with confidence intervals for the specified parameters
#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {

  # If no parameter was specified
  if (is.null(parm)) {

    # Make intervals for the model terms
    parm <- base::attr(stats::terms(object$formula), "term.labels")

    if (base::attr(stats::terms(object$formula), "intercept") == 1) {
      parm <- c("(Intercept)", parm)
    }

  }


  # Set alpha based on the chosen level
  alpha <- 1 - level


  out <- map_rbind(parm,
                   function(p) {
                     map_mean(object$estimates,
                              ~ purrr::map_dbl(., list("coef", p)) %>%
                                stats::quantile(c(alpha / 2, 1 - alpha / 2)))
                     })


  # If a vector was output, change it into a matrix
  if (is.vector(out)) {
    out <- base::as.matrix(base::t(out))
  }


  # Set the names as those of parm
  base::dimnames(out)[[1]] <- parm


  # Return out
  out
}


#' Make a prediction
#'
#' Use the model and new data to produce estimates for the response variable
#'
#' @param object A variable with the class "blblm"
#' @param new_data New values for the predictor variables to use in estimating the response
#' @param confidence Boolean for whether a confidence interval should be returned
#' @param level The significance level for the confidence itnerval (1 - alpha)
#' @param ... Included for method flexibility
#'
#' @return A single number or vector of estimates for the predictor variable (potentially as a confidence interval)
#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {

  # Reformulate the data to fit the model matrix
  X <- stats::model.matrix(
          stats::reformulate(
            base::attr(
              stats::terms(object$formula), "term.labels")
            ), new_data)


  # If a confidence interval should be used
  if (confidence) {

    # Compute a confidence interval for the response variable
    map_mean(object$estimates, ~ map_cbind(., ~ X %*% .$coef) %>%
      base::apply(1, mean_lwr_upr, level = level) %>%
      base::t())

  } else {

    # Otherwise, just return the mean response result
    map_mean(object$estimates, ~ map_cbind(., ~ X %*% .$coef) %>% base::rowMeans())

  }
}


#' Get R^2
#'
#' Get an estimate for the coefficient of determination, with the option of a confidence interval
#'
#' @param object A variable with the class "blblm"
#' @param confidence Boolean for whether a confidence interval should be reported
#' @param level The significance level for a confidence interval (1 - alpha)
#'
#' @return An estimate for R^2 (or a confidence interval vector for R^2)
#' @export
Rsq.blblm <- function(object, confidence = FALSE, level = 0.95) {

  # Use the R^2 estimates from every trial
  Rsq <- map_mean(object$estimates, ~ map_cbind(., "Rsq") %>% base::rowMeans())


  # If a confidence interval should be reported
  if (confidence) {

    # Get alpha based on the level
    alpha <- 1 - level


    # Compute the limits of the interval
    limits <- object$estimates %>%
      map_mean(~ stats::quantile(purrr::map_dbl(., "Rsq"), c(alpha / 2, 1 - alpha / 2))) %>%
      purrr::set_names(NULL)


    # Return a named vector with the results
    return(c(Rsq = Rsq, lwr = limits[1], upr = limits[2]))


  } else {

    # Otherwise, just return the sigma estimate
    return(Rsq)

  }
}


#' Get the Adjusted R^2
#'
#' Get an estimate for the adjusted coefficient of determination, with the option of a confidence interval.
#'
#' @param object A variable with the class "blblm"
#' @param confidence Boolean for whether a confidence interval should be reported
#' @param level The significance level for a confidence interval (1 - alpha)
#'
#' @return An estimate for the Adjusted R^2 (or a confidence interval vector)
#' @export
Adj.Rsq.blblm <- function(object, confidence = FALSE, level = 0.95) {

  # Use the Adjusted R^2 estimates from every trial
  Rsq <- map_mean(object$estimates, ~ map_cbind(., "Adj.Rsq") %>% base::rowMeans())


  # If a confidence interval should be reported
  if (confidence) {

    # Get alpha based on the level
    alpha <- 1 - level


    # Compute the limits of the interval
    limits <- object$estimates %>%
      map_mean(~ stats::quantile(purrr::map_dbl(., "Adj.Rsq"), c(alpha / 2, 1 - alpha / 2))) %>%
      purrr::set_names(NULL)


    # Return a named vector with the results
    return(c(Adj.Rsq = Rsq, lwr = limits[1], upr = limits[2]))


  } else {

    # Otherwise, just return the sigma estimate
    return(Rsq)

  }
}


##### Helper functions #####


# Return a confidence interval for the mean
mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = base::mean(x), stats::quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% purrr::set_names(c("lwr", "upr")))
}


# Return a mean for data in separate entries
map_mean <- function(.x, .f, ...) {
  (purrr::map(.x, .f, ...) %>% purrr::reduce(`+`)) / base::length(.x)
}


# Return a column-bound mapping
map_cbind <- function(.x, .f, ...) {
  purrr::map(.x, .f, ...) %>% purrr::reduce(cbind)
}


# Return a row-bound mapping
map_rbind <- function(.x, .f, ...) {
  purrr::map(.x, .f, ...) %>% purrr::reduce(rbind)
}
