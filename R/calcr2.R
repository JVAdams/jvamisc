#' Coefficient of Determination
#'
#' Calculate the unadjusted and adjusted coefficient of determination (R^2).
#' @param fitted
#'   A vector of fitted values.
#' @param observed
#'   A vector of observed values.
#' @param nparam
#'   A scalar, number of parameters in the fitted model.
#' @return
#'   A vector of unadjusted and adjusted coefficients of determination.
#' @export
#' @examples
#' fit1 <- lm(mpg ~ cyl + disp, data=mtcars)
#' calcr2(fit1$fitted, mtcars$mpg, 3)
#'
#' fit2 <- lm(mpg ~ cyl + disp + wt, data=mtcars)
#' calcr2(fit2$fitted, mtcars$mpg, 4)

calcr2 <- function(fitted, observed, nparam) {
  # calculate the R^2 and the adjusted R^2
  # from the fitted values, the observed values, and the number of parameters
  # formulas from Weisberg 1985 Applied Linear Regression
  resid <- observed - fitted
  ss <- function(x) {
    sum((x - mean(x))^2)
  }
  r2 <- 1 - sum(resid^2)/ss(observed)
  adjr2 <- 1 - (length(observed) - 1)/(length(observed) - nparam)*(1 - r2)
  c(r2, adjr2)
}
