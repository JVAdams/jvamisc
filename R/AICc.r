#' Comparison of models using Akaike Information Criterion
#'
#' Compares a collection of statistical models using AIC.
#' @param fitlist a list of model fits to compare, e.g., lm/glm/aov objects
#' @param corr a logical indicating whether the AIC should be corrected for small sample size, defaults to TRUE
#' @return data frame with a row for each model being compared, 
#' ordered by either the uncorrected AIC (corr=FALSE) or the AIC corrected for small sample size (corr=TRUE).
#' Columns include the number of observations (n), the number of parameters (p), the root mean squared error (rmse), 
#' the uncorrected AIC (aic), the AIC corrected for small sample size (aicc),
#' the delta AIC (daic or diacc), and the weights of evidence (aicw or aiccw).
#' @export
#' @examples 
#' fit1 <- lm(hp ~ mpg + disp + wt, data=mtcars)
#' fit2 <- lm(hp ~ mpg + disp, data=mtcars)
#' fit3 <- lm(hp ~ mpg + wt, data=mtcars)
#' fit4 <- lm(hp ~ disp + wt, data=mtcars)
#' AICc(list(fit1, fit2, fit3, fit4))

AICc <- function(fitlist, corr=TRUE) {
	modnamz <- if(is.null(names(fitlist))) paste("Model", seq(fitlist)) else names(fitlist)
	res <- data.frame(model=modnamz, n=NA, p=NA, rmse=NA, aic=NA, aicc=NA)
	for(i in seq(fitlist)) {
		fit <- fitlist[[i]]
		n <- length(fit$fitted)
		p <- n - fit$df.residual
		rmse <- sqrt(mean(fit$resid^2))
		aic <- AIC(fit)
		aicc <- aic + 2*p*(p+1)/(n-p-1)
		res[i, 2:6] <- c(n, p, rmse, aic, aicc)
		}
	if(corr) {
		res <- res[order(res$aicc), ]
		res$daicc <- res$aicc - res$aicc[1]
		edaicc <- exp(-res$daicc/2)
		res$aiccw <- edaicc/sum(edaicc)
		} else {
		res <- res[order(res$aic), ]
		res$daic <- res$aic - res$aic[1]
		edaic <- exp(-res$daic/2)
		res$aicw <- edaic/sum(edaic)
		}
	res
	}
