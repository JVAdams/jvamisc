#' Added Variable Plots of Predictors
#'
#' Produces an added variable plot given 1 response and 2 or more predictors.
#' @param Y
#'   A vector representing a single response.
#' @param X
#'   Two or more predictor columns in a matrix or data frame.
#' @param main
#'   Subtitle for the plot.
#' @return
#'   A plot is sent to the current graphics device (no value is returned).
#' @export
#' @examples
#' addedvar(Y=mtcars$hp, X=mtcars[, c("mpg", "disp", "wt")],
#'	main="Predicting horsepower from MPG, displacement, and weight")

addedvar <- function(Y, X, main="") {
	# Added variable plots of predictors
	# Y is one response vector
	# X is two or more predictor columns in a matrix or data frame
	# main is a subtitle for the plot

	# calculate the number of predictors, and assign them names
	p <- ncol(X)
	if (p < 2) {
    cat("There must be at least two predictors to construct added variable plots. \n")
	}
	if (is.null(dimnames(X)[[2]])) {
    pnames <- paste("X", 1:p, sep="")
  } else {
    pnames <- dimnames(X)[[2]]
  }
	if (any(is.na(X))) {
    cat("Observations with missing values will be excluded from procedure. \n")
	}

	# develop the full model with all predictors included
	fullmodel <- "Y ~ X[, 1]"
	if (p > 1) {
		for(i in 2:p) {
      fullmodel <- paste(fullmodel, " + X[, ", i, "]", sep="")
		}
	}

	# fit the full model with each of the predictors excluded in turn
	plotcols <- ceiling(sqrt(p - 1))
	plotrows <- ceiling(p/plotcols)
	par(mar=c(0.5, 0.5, 1, 0), mfrow=c(plotrows, plotcols), oma=c(2, 2, 2, 0),
    pty="s")
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
	mtext("Variability in xi not explained by other x!i", side=1, outer=TRUE,
    line=1, cex=1)
	mtext("Variability in y not explained by other x!i", side=2, outer=TRUE,
    line=1, cex=1, las=0)
	mtext(main, side=3, outer=TRUE, line=1, cex=1)
	}
