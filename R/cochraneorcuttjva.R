#' Cochrane-Orcutt Estimation
#'
#' Interactive method using to solve first order autocorrelation problems. This
#' procedure estimates both autocorrelation and beta coefficients recursively
#' until we reach the convergence (8th decimal). The residuals are computed
#' after estimating Beta using EGLS approach and Rho is estimated using the
#' previous residuals.
#'
#' @param reg
#'   a linear model built with lm function
#' @param convergence
#'   decimal value to reach for convergence, 8 as default
#' @param max.iter
#'   the maximum number of iterations to try before giving up on convergence,
#'   100 as default
#' @return
#'   An object of class "orcutt".  See \code{\link[orcutt]{cochrane.orcutt}}
#' @details
#'   This is a duplicate of the \code{\link[orcutt]{cochrane.orcutt}()}
#'   function from package \strong{orcutt} version 2.2, with the addition of an
#'   upper limit on the number of iterations to avoid an infinite
#'   \code{\link{while}()} loop.
#' @seealso \code{\link[orcutt]{cochrane.orcutt}}
#' @references
#'   Verbeek M. (2004) A guide to modern econometrics, John Wiley & Sons Ltd,
#'   ISBN:978-88-08-17054-5
#' @importFrom lmtest dwtest
#' @export
#' @examples
#' # example from orcutt package that converges
#'   data(icecream, package="orcutt")
#'   lm <- lm(cons ~ price + income + temp, data=icecream)
#'   cochrane.orcutt.jva(lm)
#' # another example that doesn't converge
#'   mydat <- data.frame(year=2013:2017, meas=c(14.8, 10.7, 6.2, 4, 3.9))
#'   lmfit <- lm(meas ~ year, data=mydat)
#'   cochrane.orcutt.jva(lmfit)

cochrane.orcutt.jva <- function(reg, convergence=8, max.iter=100) {
  X <- model.matrix(reg)
  Y <- model.response(model.frame(reg))
  n <- length(Y)
  e <- reg$residuals
  e2 <- e[-1]
  e3 <- e[-n]
  regP <- lm(e2 ~ e3 - 1)
  rho <- summary(regP)$coeff[1]
  rho2 <- c(rho)
  XB <- X[-1, ] - rho * X[-n, ]
  YB <- Y[-1] - rho * Y[-n]
  regCO <- lm(YB ~ XB - 1)
  ypCO <- regCO$coeff[1] + as.matrix(X[, -1]) %*% regCO$coeff[-1]
  e1 <- ypCO - Y
  e2 <- e1[-1]
  e3 <- e1[-n]
  regP <- lm(e2 ~ e3 - 1)
  rho <- summary(regP)$coeff[1]
  rho2[2] <- rho
  i <- 2
  while (round(rho2[i - 1], convergence) != round(rho2[i], convergence) &
      i <= max.iter) {
    XB <- X[-1, ] - rho * X[-n, ]
    YB <- Y[-1] - rho * Y[-n]
    regCO <- lm(YB ~ XB - 1)
    ypCO <- regCO$coeff[1] + as.matrix(X[, -1]) %*% regCO$coeff[-1]
    e1 <- ypCO - Y
    e2 <- e1[-1]
    e3 <- e1[-n]
    regP <- lm(e2 ~ e3 - 1)
    rho <- summary(regP)$coeff[1]
    i <- i + 1
    rho2[i] <- rho
  }
  regCO$number.interaction <- i - 1
  regCO$rho <- rho2[i - 1]
  if((i-1) >= max.iter) {
    regCO$coefficients <- NA
    regCO$DW <- c(lmtest::dwtest(reg)$statistic, lmtest::dwtest(reg)$p.value,
      NA, NA)
    class(regCO) <- "orcutt"
    regCO$call <- reg$call
    warning("Did not converge")
  } else {
    regCO$DW <- c(lmtest::dwtest(reg)$statistic, lmtest::dwtest(reg)$p.value,
      lmtest::dwtest(regCO)$statistic, lmtest::dwtest(regCO)$p.value)
    regF <- lm(YB ~ 1)
    tF <- anova(regCO, regF)
    regCO$Fs <- c(tF$F[2], tF$`Pr(>F)`[2])
    regCO$fitted.values <- model.matrix(reg) %*% (as.matrix(regCO$coeff))
    names(regCO$coefficients) <- colnames(X)
    regCO$std.error <- summary(regCO)$coeff[, 2]
    regCO$t.value <- summary(regCO)$coeff[, 3]
    regCO$p.value <- summary(regCO)$coeff[, 4]
    class(regCO) <- "orcutt"
    regCO$call <- reg$call
    df1 <- dim(model.frame(reg))[2] - 1
    df2 <- length(regCO$residuals) - df1 - 1
    RSS <- sum((regCO$residuals)^2)
    TSS <- sum((regCO$model[1] - mean(regCO$model[, 1]))^2)
    regCO$rse <- sqrt(RSS/df2)
    regCO$r.squared <- 1 - (RSS/TSS)
    regCO$adj.r.squared <- 1 - ((RSS/df2)/(TSS/(df1 + df2)))
    regCO$gdl <- c(df1, df2)
    regCO$rank <- df1
    regCO$df.residual <- df2
    regCO$assign <- regCO$assign[-(df1 + 1)]
    regCO$residuals <- Y - regCO$fitted.values
  }
  regCO
}
