#' Added variable plots of predictors
#'
#' Produces an added variable plot given 1 response and 2 or more predictors.
#' @param Y a vector representing a single response
#' @param X two or more predictor columns in a matrix or dataframe
#' @param main subtitle for the plot
#' @export
#' @examples added.var(Y=mtcars$hp, X=mtcars[, c("mpg", "disp", "wt")], main="Predicting horsepower from MPG, displacement, and weight")

added.var <- function(Y, X, main="") {
	# Added variable plots of predictors
	# Y is one response vector
	# X is two or more predictor columns in a matrix or dataframe
	# main is a subtitle for the plot

	# calculate the number of predictors, and assign them names
	p <- ncol(X)
	if(p < 2) cat("There must be at least two predictors to construct added variable plots. \n")
	if(is.null(dimnames(X)[[2]])) pnames <- paste("X", 1:p, sep="") else pnames <- dimnames(X)[[2]]
	if(any(is.na(X))) cat("Observations with missing values will be excluded from procedure. \n")

	# develop the full model with all predictors included
	fullmodel <- "Y ~ X[, 1]"
	if(p > 1) {
		for(i in 2:p) fullmodel <- paste(fullmodel, " + X[, ", i, "]", sep="")
		}

	# fit the full model with each of the predictors excluded in turn
	windows()
	plotcols <- ceiling(sqrt(p - 1))
	plotrows <- ceiling(p/plotcols)
	par(mar = c(2.5, 2.5, 3, 2.5), mfrow = c(plotrows, plotcols), oma = c(3, 3, 5, 0), pty="s")
	for(i in 1:p) {
		noxk <- paste("~ . - X[, ", i, "]", sep="")
		xkfory <- paste("X[, ", i, "] ~ .", sep="")
		modelnoxk <- update(formula(fullmodel), noxk)
		modelxkfory <- update(modelnoxk, xkfory)
		resy <- residuals(lm(modelnoxk, na.action=na.omit))
		resxk <- residuals(lm(modelxkfory, na.action=na.omit))
		plot(resxk, resy, main=pnames[i], xlab="", ylab="", xaxt="n", yaxt="n")
		abline(lsfit(resxk, resy))
		}
	mtext("Variability in the predictor not explained by other predictors", side=1, outer=T, line=1, cex=1)
	mtext("Variability in the response not explained by other predictors", side=2, outer=T, line=1, cex=1)
	mtext("Added variable plots for predictors", side=3, outer=T, line=3, cex=1.5)
	mtext(main, side=3, outer=T, line=1.5, cex=1)
	}
