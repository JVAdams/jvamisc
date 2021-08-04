#' Unbiased Prediction of Anti-Log Transformed Mean and Variance
#'
#' Provide unbiased estimates of the mean and the variance
#' on the original scale from an analysis of
#' variance model with a natural log transformed response.
#' @param aovfit
#'   An object of class c("aov", "lm").
#' @param xdata
#'   A data frame with predictor variables corresponding to those in
#'   \code{model} for which predictions should be made.
#' @param out
#'   A character scalar indicating what prediction value to return, either
#'   "predmean" (default), "predvar", or "predcv".
#' @return
#'   A numeric vector of predicted values on the original scale of the
#'   response.
#' @export
#' @examples
#' fit <- aov(log(yield) ~ block + N * P + K, npk)
#' mvAntilog(fit, npk)

mvAntilog <- function(aovfit, xdata, out=c("predmean", "predvar", "predcv")[1]) {
  mu <- predict(aovfit, newdata=xdata)
  sigsq <- sigma(aovfit)^2
  predmean <- exp(mu + sigsq/2)
  predvar <- exp(2*mu + 2*sigsq) - exp(2*mu + sigsq)
  predcv <- sqrt(predvar)/predmean
  if(out=="predmean") return(predmean)
  if(out=="predvar") return(predvar)
  if(out=="predcv") return(predcv)
}
