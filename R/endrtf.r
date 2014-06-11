#' Write and Close an RTF Document
#'
#' Write and close an rtf (rich text format) document.
#' @param rtf		An rtf object, default \code{doc}.
#' @param details	Logical scalar indicating if session details should be added to the end of the document, default FALSE.
#' @param ...		Additional parameters to \code{\link[rtf]{addPageBreak}}.
#' @seealso			\code{\link[jvamisc]{startrtf}}, \code{\link[jvamisc]{heading}}, \code{\link[jvamisc]{para}}, \code{\link[jvamisc]{tabl}}, 
#' \code{\link[jvamisc]{figu}}, \code{\link[jvamisc]{figbig}}, \code{\link[rtf]{RTF}}, \code{\link[rtf]{addPageBreak}}.
#' @import 			rtf
#' @export
#' @examples 
#' \dontrun{
#' doc <- startrtf()
#' heading("Heading 1")
#' para("First paragraph.")
#' tab <- head(cars)
#' tabl("First few rows of cars data.", row.names=FALSE)
#' heading("Heading 2", 2)
#' para("Second paragraph.")
#' fig <- function() {
#' 	plot(cars)
#' 	lo <- loess(cars$dist ~ cars$speed)
#' 	lines(lo$x, lo$fitted)
#' 	}
#' figu("Speed vs. distance from the cars data.")
#' endrtf()
#' }

endrtf <- function(rtf=doc, details=FALSE, ...) {
	if(details==TRUE) {
		addPageBreak(rtf, ...)
		addSessionInfo(rtf)
		}
	done(rtf)
	}
