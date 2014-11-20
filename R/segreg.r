#' Segmented Regression
#'
#' Fit a simple linear segmented regression (also known as changepoint, hockey stick, or broken line regression).  
#' @param x 	Numeric vector, the independent variable which will be "broken".
#' @param y 	Numeric vector, the dependent variable which will be used to define the break point.
#' @param k 	Numeric scalar, the location of the break point, if known.  If NULL, the default, the optimal break point will be chosen from among all
#' unique values of x except for the minimum and the maximum. 
#' @return 		List with three elements, fit: the resulting lm object, k, and pred: data frame with the unique values of x (and k) with predicted values.
#' @export
#' @details		The break point cuts the x vector into two groups, x <= k and x > k.
#' @examples 
#' indvar <- sample(1:50, 100, replace=TRUE)
#' depvar <- ifelse(indvar<32, indvar + 3, indvar/4 + 27) + rnorm(100, sd=2)
#' sr <- segreg(indvar, depvar)
#' plot(indvar, depvar)
#' lines(sr$pred)
#' abline(v=sr$k, lty=2)

segreg <- function(x, y, k=NULL) {
	if(is.null(k)) {
		rsscp <- function(K, X, Y) {
			obj <- lm(Y ~ X + I((X>K)*(X-K)))
			sum(obj$residuals^2)
			}
		xes <- sort(unique(x))
		somexes <- xes[2:(length(xes)-1)]
		k <- optimize(rsscp, interval=somexes, X=x, Y=y)[[1]]
		}
	z <- I((x>k)*(x-k))
	class(z) <- "numeric"
	fit <- lm(y ~ x + z)
	xk <- c(x, k)
	newd <- data.frame(x=xk[!duplicated(xk)], z=c(z, 0)[!duplicated(xk)])
	newd <- newd[order(newd$x), ]
	p <- predict(fit, newdata=newd)
	list(fit=fit, k=k, pred=data.frame(x=newd$x, y=p))
	}
