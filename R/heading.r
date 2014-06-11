#' Add a Heading to an RTF Document
#'
#' Add a text heading to an rtf (rich text format) document.
#' @param words 	Character scalar text of heading to add to document.
#' @param htype 	Integer scalar heading type, 1 = bold and font size 12, 2 = bold and font size 10, 3 = italics and font size 10, default 1.
#' @param rtf		An rtf object, default \code{doc}.
#' @details 		The specified heading is written to the rtf file.
#' @seealso			\code{\link[jvamisc]{startrtf}}, \code{\link[jvamisc]{para}}, \code{\link[jvamisc]{tabl}}, \code{\link[jvamisc]{figu}}, 
#' \code{\link[jvamisc]{figbig}}, \code{\link[jvamisc]{endrtf}}, \code{\link[rtf]{RTF}}.
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

heading <- function(words, htype=1, rtf=doc) {
	if(htype==1) {
		addHeader(this=rtf, title=words, font.size=12)
		} else {
		startParagraph(this=rtf)
		addText(this=rtf, words, bold=c(TRUE, FALSE)[htype-1], italic=c(FALSE, TRUE)[htype-1])
		endParagraph(this=rtf)
		addNewLine(this=rtf)
		}
	}
