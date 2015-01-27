#' Cross Validated Tree Model
#'
#' Fit a classification or regression tree model to data, and automatically prune the tree based on the cross-validation error.
#' @param ... 	Arguments provided to \code{\link{rpart}} including formula (the only required argument).
#' @return 		An object of class rpart. See \code{\link{rpart.object}}.
#' @import		rpart
#' @export
#' @details		First a tree model is fit.  Then the model is pruned such that it has the fewest number of splits that 
#'	yield a mean cross-validation error less than the minimum cross-validation error plus one standard error.
#' @examples 
#' fit <- jvatree(Ozone ~ ., data=airquality)
#' par(xpd=NA) # otherwise on some devices the text is clipped
#' plot(fit)
#' text(fit, use.n = TRUE)

jvatree <- function(...) {
	tree1 <- rpart(...)
	tab <- tree1$cptable
	xerror <- tab[, "xerror"]
	xstd <- tab[, "xstd"]
	CP <- tab[, "CP"]
	indx.min.xerror <- which.min(xerror)
	chosen.cp <- CP[xerror < (xerror + xstd)[indx.min.xerror]][1]
	tree2 <- prune(tree1, cp=chosen.cp)
	tree2
	}
