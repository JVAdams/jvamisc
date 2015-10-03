#' Unbiased Prediction of Log Transformed Response on Original Scale
#'
#' Provide unbiased estimates on the original scale from an analysis of
#' variance model with a log transformed response.
#' @param aovfit
#'   An object of class c("aov", "lm").
#' @param xdata
#'   A data frame with predictor variables corresponding to those in
#'   \code{model} for which predictions should be made.
#' @param logbase
#'   A numeric scalar, the base of the log transformation used in
#'   the transformed response of \code{model}, default exp(1).
#' @param k
#'   A numeric scalar, the constant added to the response prior to
#'   transformation, default 0.
#' @return
#'   A numeric vector of predicted values on the original scale of the
#'   response.
#' @export
#' @examples
#' fit <- aov(log(yield) ~ block + N * P + K, npk)
#' predAntilog(fit, npk)

predAntilog <- function(aovfit, xdata, logbase=exp(1), k=0) {
  mse <- rev(as.matrix(summary(aovfit)[[1]])[, "Mean Sq"])[1]
  plpe <- predict(aovfit, newdata=xdata, se.fit=T)
  m <- plpe$fit
  v <- plpe$se.fit^2 + mse
  logbase^(m + v/2) - k
  }
