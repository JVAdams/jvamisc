#' Plot Data Frame
#'
#' Plots each variable of a data frame.
#' @param df 	A data frame.
#' @param one 	A logical, whether to display all plots in a single graphics device, default TRUE.
#' @param ... 	Other parameters to \code{par(...)}.
#' @return 		A data frame with summary statistics for each variable in the data frame, number entered, 
#' number missing, number unique, minimum, mean, maximum, and the ratio of the maximum over the non-zero minimum.
#' @export
#' @seealso 	\code{\link{par}}.
#' @examples plotdf(mtcars)

plotdf <- function(df, one=TRUE, ...) {
	if(one) dev.new(record=TRUE)
	par(...)
	mcex <- 1 #as.numeric(par("cex"))*2
	print(mcex)
	# plot the columns of a data frame
	for(i in 1:dim(df)[2]) {
		x <- df[[i]]
		if(sum(!is.na(x))>0) {
			name <- names(df)[i]
			if(is.factor(x)) plot(x, main=name, cex.main=mcex) else
			if(is.character(x) & length(unique(x))<=50) plot(as.factor(x), main=name, cex.main=mcex) else
			if(is.numeric(x) & sum(is.finite(x)) < 2) plot(1, 1, type="n", xlab="", ylab="", axes=FALSE, 
				main=paste(name, ": numeric vector w/ < 2 finite values", sep=""), cex.main=mcex) else
			if(is.numeric(x) & length(unique(x))<=10) plot(as.factor(x), main=name, cex.main=mcex) else
			if(is.numeric(x)) plot.default(x[is.finite(x)], main=name, cex.main=mcex) else
			if(is.character(x)) plot(1, 1, type="n", xlab="", ylab="", axes=FALSE, main=paste(name, ": character vector w/ > 50 unique values", sep=""), cex.main=mcex)
			}
		}
	# print summary for each field
	stats <- t(sapply(df, function(x) if(is.numeric(x) | is.integer(x)) {
		c(min=min(x, na.rm=TRUE), mean=mean(x, na.rm=TRUE), max=max(x, na.rm=TRUE), 
			maxmin=max(abs(x)[!is.na(x) & abs(x)>0])/min(abs(x)[!is.na(x) & abs(x)>0]))
		} else {
		rep(-999, 4)
		}
		))
	no.unique <- sapply(df, function(x) length(table(x)))
	no.entered <- sapply(df, function(x) if(is.character(x)) sum(x!="") else sum(!is.na(x)))
	no.missing <- sapply(df, function(x) if(is.character(x)) sum(x=="") else sum(is.na(x)))
	stats[no.entered<1] <- NA
	out <- data.frame(no.entered, no.missing, no.unique, stats)
	print(out)
	answer <- readline("Do you want to delete the plots? ")
	if (answer == "y") {
		dev.off(which = dev.cur())
		.SavedPlots <<- NULL
		cat("Plots deleted.\n")
		} else cat("Okay.  If you want to delete them later, use\n\n.SavedPlots <- NULL\n")
	invisible(out)
	}
